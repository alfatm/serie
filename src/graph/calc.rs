use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::git::{Commit, CommitHash, Repository};

type CommitPosMap = FxHashMap<CommitHash, (usize, usize)>;

#[derive(Debug)]
pub struct Graph {
    pub commits: Vec<Rc<Commit>>,
    pub commit_pos_map: CommitPosMap,
    pub edges: Vec<Vec<Edge>>,
    pub max_pos_x: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Edge {
    pub edge_type: EdgeType,
    pub pos_x: usize,
    pub associated_line_pos_x: usize,
}

impl Edge {
    pub fn new(edge_type: EdgeType, pos_x: usize, line_pos_x: usize) -> Self {
        Self {
            edge_type,
            pos_x,
            associated_line_pos_x: line_pos_x,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum EdgeType {
    Vertical,    // │
    Horizontal,  // ─
    Up,          // ╵
    Down,        // ╷
    Left,        // ╴
    Right,       // ╶
    RightTop,    // ╮
    RightBottom, // ╯
    LeftTop,     // ╭
    LeftBottom,  // ╰
}

pub fn calc_graph(repository: &Repository) -> Graph {
    let commits = repository.all_commits();

    let commit_pos_map = calc_commit_positions(&commits, repository);
    let (graph_edges, max_pos_x) = calc_edges(&commit_pos_map, &commits, repository);

    Graph {
        commits,
        commit_pos_map,
        edges: graph_edges,
        max_pos_x,
    }
}

/// New algorithm for commit positioning that places the primary branch (master/main)
/// in column 0, with feature branches to the right.
///
/// The algorithm:
/// 1. Track active branch lines as we iterate through commits (top to bottom)
/// 2. A new branch line starts when a commit has no children (first-parent relationship)
/// 3. The primary line (following first parents) stays in column 0
/// 4. Secondary branches get columns to the right
fn calc_commit_positions(commits: &[Rc<Commit>], repository: &Repository) -> CommitPosMap {
    let mut commit_pos_map: CommitPosMap = FxHashMap::default();

    // Track which column each active branch line occupies
    // Each element is Some(commit_hash) where commit_hash is the "current" commit on that line
    // or None if the column is free
    let mut active_columns: Vec<Option<CommitHash>> = Vec::new();

    // Map from commit hash to its assigned column
    let mut commit_to_column: FxHashMap<CommitHash, usize> = FxHashMap::default();

    for (pos_y, commit) in commits.iter().enumerate() {
        let hash = &commit.commit_hash;

        // Find children that have this commit as their first parent
        // These are the "continuing" lines from above
        let first_parent_children: Vec<&CommitHash> = repository
            .children_hash(hash)
            .into_iter()
            .filter(|child_hash| {
                let child_parents = repository.parents_hash(child_hash);
                !child_parents.is_empty() && *child_parents[0] == *hash
            })
            .collect();

        if first_parent_children.is_empty() {
            // This commit starts a new branch line (no children have it as first parent)
            // Find the first available column, preferring column 0 for the first branch
            let pos_x = find_first_vacant_column(&active_columns);
            occupy_column(&mut active_columns, pos_x, hash.clone());
            commit_to_column.insert(hash.clone(), pos_x);
            commit_pos_map.insert(hash.clone(), (pos_x, pos_y));
        } else {
            // This commit continues from one or more child branches
            // Find which columns those children occupy and pick the leftmost (smallest)
            let mut min_col = usize::MAX;
            let mut columns_to_free: Vec<usize> = Vec::new();

            for child_hash in &first_parent_children {
                if let Some(&col) = commit_to_column.get(*child_hash) {
                    columns_to_free.push(col);
                    if col < min_col {
                        min_col = col;
                    }
                }
            }

            // Free all columns except the one we'll use
            for col in &columns_to_free {
                if *col != min_col {
                    free_column(&mut active_columns, *col);
                }
            }

            // If we didn't find any column (shouldn't happen), get a new one
            if min_col == usize::MAX {
                min_col = find_first_vacant_column(&active_columns);
            }

            // Occupy the column with this commit
            occupy_column(&mut active_columns, min_col, hash.clone());
            commit_to_column.insert(hash.clone(), min_col);
            commit_pos_map.insert(hash.clone(), (min_col, pos_y));
        }
    }

    commit_pos_map
}

fn find_first_vacant_column(columns: &[Option<CommitHash>]) -> usize {
    columns
        .iter()
        .position(|c| c.is_none())
        .unwrap_or(columns.len())
}

fn occupy_column(columns: &mut Vec<Option<CommitHash>>, col: usize, hash: CommitHash) {
    if col >= columns.len() {
        columns.resize(col + 1, None);
    }
    columns[col] = Some(hash);
}

fn free_column(columns: &mut [Option<CommitHash>], col: usize) {
    if col < columns.len() {
        columns[col] = None;
    }
}

#[derive(Debug, Clone)]
struct WrappedEdge {
    edge: Edge,
    edge_parent_hash: CommitHash,
}

impl WrappedEdge {
    fn new(
        edge_type: EdgeType,
        pos_x: usize,
        line_pos_x: usize,
        edge_parent_hash: CommitHash,
    ) -> Self {
        Self {
            edge: Edge::new(edge_type, pos_x, line_pos_x),
            edge_parent_hash,
        }
    }
}

fn calc_edges(
    commit_pos_map: &CommitPosMap,
    commits: &[Rc<Commit>],
    repository: &Repository,
) -> (Vec<Vec<Edge>>, usize) {
    let mut max_pos_x = 0;
    let mut edges: Vec<Vec<WrappedEdge>> = vec![vec![]; commits.len()];

    for commit in commits {
        let (pos_x, pos_y) = commit_pos_map[&commit.commit_hash];
        let hash = &commit.commit_hash;

        for child_hash in repository.children_hash(hash) {
            let (child_pos_x, child_pos_y) = commit_pos_map[child_hash];

            if pos_x == child_pos_x {
                // commit
                edges[pos_y].push(WrappedEdge::new(EdgeType::Up, pos_x, pos_x, hash.clone()));
                for y in ((child_pos_y + 1)..pos_y).rev() {
                    edges[y].push(WrappedEdge::new(
                        EdgeType::Vertical,
                        pos_x,
                        pos_x,
                        hash.clone(),
                    ));
                }
                edges[child_pos_y].push(WrappedEdge::new(
                    EdgeType::Down,
                    pos_x,
                    pos_x,
                    hash.clone(),
                ));
            } else {
                let child_first_parent_hash = &commits[child_pos_y].parent_commit_hashes[0];
                if *child_first_parent_hash == *hash {
                    // branch
                    if pos_x < child_pos_x {
                        edges[pos_y].push(WrappedEdge::new(
                            EdgeType::Right,
                            pos_x,
                            child_pos_x,
                            hash.clone(),
                        ));
                        for x in (pos_x + 1)..child_pos_x {
                            edges[pos_y].push(WrappedEdge::new(
                                EdgeType::Horizontal,
                                x,
                                child_pos_x,
                                hash.clone(),
                            ));
                        }
                        edges[pos_y].push(WrappedEdge::new(
                            EdgeType::RightBottom,
                            child_pos_x,
                            child_pos_x,
                            hash.clone(),
                        ));
                    } else {
                        edges[pos_y].push(WrappedEdge::new(
                            EdgeType::Left,
                            pos_x,
                            child_pos_x,
                            hash.clone(),
                        ));
                        for x in (child_pos_x + 1)..pos_x {
                            edges[pos_y].push(WrappedEdge::new(
                                EdgeType::Horizontal,
                                x,
                                child_pos_x,
                                hash.clone(),
                            ));
                        }
                        edges[pos_y].push(WrappedEdge::new(
                            EdgeType::LeftBottom,
                            child_pos_x,
                            child_pos_x,
                            hash.clone(),
                        ));
                    }
                    for y in ((child_pos_y + 1)..pos_y).rev() {
                        edges[y].push(WrappedEdge::new(
                            EdgeType::Vertical,
                            child_pos_x,
                            child_pos_x,
                            hash.clone(),
                        ));
                    }
                    edges[child_pos_y].push(WrappedEdge::new(
                        EdgeType::Down,
                        child_pos_x,
                        child_pos_x,
                        hash.clone(),
                    ));
                } else {
                    // merge
                    // skip
                }
            }
        }

        if max_pos_x < pos_x {
            max_pos_x = pos_x;
        }
    }

    for commit in commits {
        let (pos_x, pos_y) = commit_pos_map[&commit.commit_hash];
        let hash = &commit.commit_hash;

        for child_hash in repository.children_hash(hash) {
            let (child_pos_x, child_pos_y) = commit_pos_map[child_hash];

            if pos_x == child_pos_x {
                // commit
                // skip
            } else {
                let child_first_parent_hash = &commits[child_pos_y].parent_commit_hashes[0];
                if *child_first_parent_hash == *hash {
                    // branch
                    // skip
                } else {
                    // merge
                    let mut overlap = false;
                    let mut new_pos_x = pos_x;

                    let mut skip_judge_overlap = true;
                    for y in (child_pos_y + 1)..pos_y {
                        let processing_commit_pos_x =
                            commit_pos_map.get(&commits[y].commit_hash).unwrap().0;
                        if processing_commit_pos_x == new_pos_x {
                            skip_judge_overlap = false;
                            break;
                        }
                        if edges[y]
                            .iter()
                            .filter(|e| e.edge.pos_x == pos_x)
                            .filter(|e| matches!(e.edge.edge_type, EdgeType::Vertical))
                            .any(|e| &e.edge_parent_hash != hash)
                        {
                            skip_judge_overlap = false;
                            break;
                        }
                    }

                    if !skip_judge_overlap {
                        for y in (child_pos_y + 1)..pos_y {
                            let processing_commit_pos_x =
                                commit_pos_map.get(&commits[y].commit_hash).unwrap().0;
                            if processing_commit_pos_x == new_pos_x {
                                overlap = true;
                                if new_pos_x < processing_commit_pos_x + 1 {
                                    new_pos_x = processing_commit_pos_x + 1;
                                }
                            }
                            for edge in &edges[y] {
                                if edge.edge.pos_x >= new_pos_x
                                    && &edge.edge_parent_hash != hash
                                    && matches!(edge.edge.edge_type, EdgeType::Vertical)
                                {
                                    overlap = true;
                                    if new_pos_x < edge.edge.pos_x + 1 {
                                        new_pos_x = edge.edge.pos_x + 1;
                                    }
                                }
                            }
                        }
                    }

                    // Merge lines use pos_x as the color (source branch color)
                    // The merge line should have the color of the branch being merged FROM
                    let merge_color = pos_x;

                    if overlap {
                        // detour
                        edges[pos_y].push(WrappedEdge::new(
                            EdgeType::Right,
                            pos_x,
                            merge_color,
                            hash.clone(),
                        ));
                        for x in (pos_x + 1)..new_pos_x {
                            edges[pos_y].push(WrappedEdge::new(
                                EdgeType::Horizontal,
                                x,
                                merge_color,
                                hash.clone(),
                            ));
                        }
                        edges[pos_y].push(WrappedEdge::new(
                            EdgeType::RightBottom,
                            new_pos_x,
                            merge_color,
                            hash.clone(),
                        ));
                        for y in ((child_pos_y + 1)..pos_y).rev() {
                            edges[y].push(WrappedEdge::new(
                                EdgeType::Vertical,
                                new_pos_x,
                                merge_color,
                                hash.clone(),
                            ));
                        }
                        edges[child_pos_y].push(WrappedEdge::new(
                            EdgeType::RightTop,
                            new_pos_x,
                            merge_color,
                            hash.clone(),
                        ));
                        for x in (child_pos_x + 1)..new_pos_x {
                            edges[child_pos_y].push(WrappedEdge::new(
                                EdgeType::Horizontal,
                                x,
                                merge_color,
                                hash.clone(),
                            ));
                        }
                        edges[child_pos_y].push(WrappedEdge::new(
                            EdgeType::Right,
                            child_pos_x,
                            merge_color,
                            hash.clone(),
                        ));

                        if max_pos_x < new_pos_x {
                            max_pos_x = new_pos_x;
                        }
                    } else {
                        edges[pos_y].push(WrappedEdge::new(
                            EdgeType::Up,
                            pos_x,
                            merge_color,
                            hash.clone(),
                        ));
                        for y in ((child_pos_y + 1)..pos_y).rev() {
                            edges[y].push(WrappedEdge::new(
                                EdgeType::Vertical,
                                pos_x,
                                merge_color,
                                hash.clone(),
                            ));
                        }
                        if pos_x < child_pos_x {
                            edges[child_pos_y].push(WrappedEdge::new(
                                EdgeType::LeftTop,
                                pos_x,
                                merge_color,
                                hash.clone(),
                            ));
                            for x in (pos_x + 1)..child_pos_x {
                                edges[child_pos_y].push(WrappedEdge::new(
                                    EdgeType::Horizontal,
                                    x,
                                    merge_color,
                                    hash.clone(),
                                ));
                            }
                            edges[child_pos_y].push(WrappedEdge::new(
                                EdgeType::Left,
                                child_pos_x,
                                merge_color,
                                hash.clone(),
                            ));
                        } else {
                            edges[child_pos_y].push(WrappedEdge::new(
                                EdgeType::RightTop,
                                pos_x,
                                merge_color,
                                hash.clone(),
                            ));
                            for x in (child_pos_x + 1)..pos_x {
                                edges[child_pos_y].push(WrappedEdge::new(
                                    EdgeType::Horizontal,
                                    x,
                                    merge_color,
                                    hash.clone(),
                                ));
                            }
                            edges[child_pos_y].push(WrappedEdge::new(
                                EdgeType::Right,
                                child_pos_x,
                                merge_color,
                                hash.clone(),
                            ));
                        }
                    }
                }
            }
        }

        if max_pos_x < pos_x {
            max_pos_x = pos_x;
        }
    }

    let edges: Vec<Vec<Edge>> = edges
        .into_iter()
        .map(|es| {
            let mut es: Vec<Edge> = es.into_iter().map(|e| e.edge).collect();
            es.sort_by_key(|e| (e.associated_line_pos_x, e.pos_x, e.edge_type));
            es.dedup();
            es
        })
        .collect();

    (edges, max_pos_x)
}
