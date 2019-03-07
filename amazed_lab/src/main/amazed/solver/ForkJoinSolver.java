package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver
{
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch(){
        return null;
    }


    class PlayerThread implements Runnable {

        @Override
        public void run() {
            int currentNode = start;
            List<Integer> result = new ArrayList<>();
            while(true){
                result.add(currentNode);
                if (visited.contains(start)){
                    result = null;
                    break;
                }
                else if (maze.hasGoal(start)){
                    break;
                }
                else{
                    List<Integer> neighbours = new ArrayList<>();

                    for (int nb: maze.neighbors(currentNode)) {
                        // if nb has not been already visited,
                        // nb can be reached from current (i.e., current is nb's predecessor)
                        if (!visited.contains(nb))
                            neighbours.add(nb);
                        predecessor.put(nb, currentNode);
                    }
                    if(neighbours.size() == 0){
                        result = null;
                        break;
                    }
                    else if(neighbours.size() == 1){
                        currentNode = neighbours.get(0);
                    }
                    else{
                        for (int node : neighbours){
                            //spawn process for node

                        }
                    }
                }
                break;
            }
        }
    }
}

