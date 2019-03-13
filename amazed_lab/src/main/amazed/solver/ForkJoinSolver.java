package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver {

    static private AtomicBoolean running = new AtomicBoolean(true);
    static private ConcurrentSkipListSet<Integer> concurrentVisited = new ConcurrentSkipListSet<>();
    static private Maze staticMaze;
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
        staticMaze = maze;
        this.forkAfter = forkAfter;
    }

    public ForkJoinSolver(int start) {
        this(staticMaze);
        this.start = start;
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

    /**
     * Steps through the maze, searching for the goal. Forks when presented with more than
     * one path.
     *
     * @return a list containing the path from start to goal, or null if goal is not found
     */
    private List<Integer> parallelSearch() {

        if (frontier.empty()) {
            frontier.push(start);
        }
        int player = staticMaze.newPlayer(start);
        while (!frontier.isEmpty() && running.get()) {
            int currentNode = frontier.pop();
            if (!concurrentVisited.contains(currentNode)) {
                staticMaze.move(player, currentNode);
            }
            if (staticMaze.hasGoal(currentNode)) {
                List<Integer> res = pathFromTo(start, currentNode);
                running.set(false);
                return res;
            } else {
                concurrentVisited.add(currentNode);
                checkNodeNeighbours(currentNode);
                if(frontier.size() > 1) {
                   while(!frontier.isEmpty()) {
                       int neighbour = frontier.pop();
                       spawnChildSolvers(neighbour);
                   }
                   return joinChildren(currentNode);
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
            ForkJoinSolver child = new ForkJoinSolver(currentNode);
            concurrentVisited.add(currentNode);
            childProcesses.add(child);
            child.fork();
        }

    /**
     * Joins all child processes that were created by this process
     *
     * @param current the current node of the process
     * @return a path from goal to node current or null if goal was not found
     */
    private List<Integer> joinChildren(int current){
            for (ForkJoinSolver child : childProcesses) {
                List<Integer> results;
                    results = child.join();
                if (results != null) {
                    List<Integer> concatResult = pathFromTo(start, current);
                    concatResult.addAll(results);
                    return concatResult;
                }
            }
            return null;
    }

    /**
     * Pushes all neighbours of a node to frontier and predecessor
     *
     * @param node the node to check the neighbours on
     */
    private void checkNodeNeighbours( int node){
        Set<Integer> neighbours = staticMaze.neighbors(node);
        for (int neighbour : neighbours) {
            if (!concurrentVisited.contains(neighbour)) {
                predecessor.put(neighbour, node);
                frontier.push(neighbour);
            }
        }
    }
}

