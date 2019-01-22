import TSim.*;

/*
  TODO: Implement train
  TODO: Implement sensors
  TODO: Implement some form of semaphore
  TODO: Implement handling of SensorEvents
  TODO: Implement handling of switches
  TODO: Evaluate if current sensor placements can be improved

 */

public class Lab1 implements Runnable {

  private TSimInterface tsi;

  public Lab1(int speed1, int speed2) {
    tsi = TSimInterface.getInstance();

    try {
      tsi.setDebug(true);
      tsi.setSpeed(1, speed1);
    } catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }

    try {
      tsi.setSpeed(2, speed2);
    } catch (CommandException e) {
      e.printStackTrace();
      System.exit(2);
    }


  }
  // TODO: Implement a representation of the train
  class Train extends Thread {

  }

  @Override
  public void run() {
    boolean[] areaList = new boolean[7];
    while (true) {
      try {
        SensorEvent event = tsi.getSensor(1);
        int current = 0;
        switch (event.getXpos()+event.getYpos()) {
          case 1:
            current = 1;
            break;
          case 2:
            current = 2;
          case 3:

          case 4:

          case 5:

          case 6:

        }
          if (areaList[current] == true) {

          }


      } catch (CommandException e) {
        e.printStackTrace();    // or only e.getMessage() for the error
        System.exit(1);
      } catch (InterruptedException e) {
        e.getMessage();
        System.exit(1);
      }

    }
  }
}
