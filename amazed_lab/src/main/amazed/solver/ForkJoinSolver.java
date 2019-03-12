package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ExecutionException;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver {

    private List<ForkJoinSolver> childProcesses = new ArrayList<>();

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
    }

    public ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited ) {
        this(maze);
        this.forkAfter = forkAfter;
        this.visited = visited;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return the list of node identifiers from the start node to a
     * goal node in the maze; <code>null</code> if such a path cannot
     * be found.
     */
    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {

        if (frontier.empty()) {
            frontier.push(start);
        }
        while (!frontier.isEmpty()) {
            int currentNode = frontier.pop();
            int player = maze.newPlayer(currentNode);
            maze.move(player, currentNode);

            if (maze.hasGoal(currentNode)) {
                return pathFromTo(start, currentNode);
            } else {
                visited.add(currentNode);
                checkNodeNeighbours(currentNode);
                if(frontier.size() > 1) {
                   while(!frontier.isEmpty()) {
                       int child = frontier.pop();
                       spawnChildSolvers(child);
                   }
                   return joinChildren();
                }
            }
        }
            return null;
    }

        /**
         * Spawns a child process which starts at the given node
         *
         * @param currentNode the node to start on
         */
        private void spawnChildSolvers(int currentNode){
            ForkJoinSolver child = new ForkJoinSolver(maze, forkAfter, visited);
            childProcesses.add(child);
            child.frontier.push(currentNode);
            child.fork();
        }

        private List<Integer> joinChildren(){
            for (ForkJoinSolver child : childProcesses) {
                List<Integer> results = null;
                try {
                    results = child.get();
                } catch (ExecutionException e) {
                    e.printStackTrace();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                if (results != null) {
                    return results;
                }
            }
            return null;
        }

        private void checkNodeNeighbours( int node){
            Set<Integer> neighbours = maze.neighbors(node);

            for (int neighbour : neighbours) {
                if (!visited.contains(neighbour)) {
                    predecessor.put(neighbour, node);
                    frontier.push(neighbour);
                }
            }
        }
    }

