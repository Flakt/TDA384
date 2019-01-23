import TSim.*;

import java.util.ArrayList;
import java.util.List;

/*
  TODO: Implement train
  TODO: Implement sensors
  TODO: Implement some form of semaphore
  TODO: Implement handling of SensorEvents
  TODO: Implement handling of switches
  TODO: Evaluate if current sensor placements can be improved

 */

public class Lab1 {

  enum SwitchName {
    NORTH,SOUTH,PITSTOP_EAST,PITSTOP_WEST
  }

  enum SensorName {
    NORTH_STATION,NORTH_OF_NORTH_STATION,JUNCTION_WEST,JUNCTION_NORTH,JUNCTION_EAST,JUNCTION_SOUTH,
    EAST_OF_NORTH_SWITCH,WEST_OF_PITSTOP_EAST,SOUTHWEST_OF_PITSTOP_EAST,WEST_OF_PITSTOP_WEST,WEST_OF_SOUTH_STATION,
    SOUTHWEST_OF_SOUTH_STATION,SOUTH_STATION,SOUTH_OF_SOUTH_STATION
  }

  private TSimInterface tsi;

  private List<Switch> switches = new ArrayList<>();

  private List<Sensor> sensors = new ArrayList<>();


  public Lab1(int speed1, int speed2) {
    tsi = TSimInterface.getInstance();
    Train train1 = new Train(1,speed1,tsi);
    Train train2 = new Train(2,speed2,tsi);

    switches.add(new Switch(17,7,SwitchName.NORTH));
    switches.add(new Switch(3,11,SwitchName.SOUTH));
    switches.add(new Switch(15,9,SwitchName.PITSTOP_EAST));
    switches.add(new Switch(4,9,SwitchName.PITSTOP_WEST));

    sensors.add(new Sensor(15,3,SensorName.NORTH_OF_NORTH_STATION));
    sensors.add(new Sensor(15,5,SensorName.NORTH_STATION));
    sensors.add(new Sensor(6,5,SensorName.JUNCTION_WEST));
    sensors.add(new Sensor(10,5,SensorName.JUNCTION_NORTH));
    sensors.add(new Sensor(12,7,SensorName.JUNCTION_EAST));
    sensors.add(new Sensor(12,8,SensorName.JUNCTION_SOUTH));
    sensors.add(new Sensor(19,9,SensorName.EAST_OF_NORTH_SWITCH));
    sensors.add(new Sensor(9,9,SensorName.WEST_OF_PITSTOP_EAST));
    sensors.add(new Sensor(9,10,SensorName.SOUTHWEST_OF_PITSTOP_EAST));
    sensors.add(new Sensor(1,10,SensorName.WEST_OF_PITSTOP_WEST));
    sensors.add(new Sensor(5,11,SensorName.WEST_OF_SOUTH_STATION));
    sensors.add(new Sensor(5,13,SensorName.SOUTHWEST_OF_SOUTH_STATION));
    sensors.add(new Sensor(16,11,SensorName.SOUTH_STATION));
    sensors.add(new Sensor(16,13,SensorName.SOUTH_OF_SOUTH_STATION));

  }

  class Switch {
    int x;
    int y;
    SwitchName switchName;

    Switch (int x, int y, SwitchName switchName) {
      this.x = x;
      this.y = y;
      this.switchName = switchName;
    }
  }

  class Sensor {
    int x;
    int y;
    SensorName sensorName;

    Sensor (int x, int y, SensorName sensorName) {
      this.x = x;
      this.y = y;
      this.sensorName = sensorName;
    }
  }

  // TODO: Implement a representation of the train
  class Train extends Thread {
    boolean forward;
    int velocity;
    int id;
    TSimInterface tsi;
    int maxVelocity;

    Train(int id, int startVelocity, TSimInterface tsi) {
      this.id = id;
      this.forward = true;
      this.velocity = checkVelocity(startVelocity);
      this.tsi = tsi;
      this.maxVelocity = 15;
      try {
        tsi.setSpeed(id,startVelocity);
      } catch (CommandException e) {
        e.printStackTrace();
        System.exit(1);
      }
    }

    private int checkVelocity(int startVelocity) {
      if (startVelocity <= maxVelocity) {
        return startVelocity;
      }
      else {
        return maxVelocity;
      }
    }

    private void activateBreak() throws CommandException {
      tsi.setSpeed(id,0);
    }

    private void goForward() throws CommandException {
      if (forward) {
        tsi.setSpeed(id, velocity);
      }
      else {
        tsi.setSpeed(id, -velocity);
      }
    }

    private void changeDirection() throws CommandException {
      forward = !forward;
    }

    private void waitAtStation() {
      try {
        tsi.setSpeed(id, 0);
        sleep(1000 + (20 * velocity));
        changeDirection();
        tsi.setSpeed(id, velocity);
      }
      catch (CommandException e) {
        e.printStackTrace();
        System.exit(1);
      }
      catch (InterruptedException e) {
        e.printStackTrace();
        System.exit(1);
      }
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

}
