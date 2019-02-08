import TSim.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;

import static java.util.Arrays.asList;

public class Lab1 {

  //All of the enums below are used purely to make the code more readable and maintainable in many cases removing the need for comments.  
  enum SwitchName {
    NORTH_STATION_SWITCH, SOUTH_STATION_SWITCH, MIDDLE_SWITCH_EAST, MIDDLE_SWITCH_WEST
  }
  
  enum SensorName {
    SOUTH_OF_NORTH_STATION,NORTH_OF_NORTH_STATION, WEST_OF_CROSSING, NORTH_OF_CROSSING, EAST_OF_CROSSING, SOUTH_OF_CROSSING,
    SOUTHWEST_OF_NORTH_STATION_SWITCH, EAST_OF_NORTH_STATION_SWITCH, WEST_OF_NORTH_STATION_SWITCH, SOUTHEAST_OF_SOUTH_STATION_SWITCH,
    EAST_OF_SOUTH_STATION_SWITCH, WEST_OF_SOUTH_STATION_SWITCH, NORTH_OF_SOUTH_STATION, SOUTH_OF_SOUTH_STATION, WEST_OF_MIDDLE_SWITCH_EAST,
    EAST_OF_MIDDLE_SWITCH_EAST, SOUTHWEST_OF_MIDDLE_SWITCH_EAST, WEST_OF_MIDDLE_SWITCH_WEST, EAST_OF_MIDDLE_SWITCH_WEST,
    SOUTHEAST_OF_MIDDLE_SWITCH_WEST
  }

  enum SemaphoreName {
    NORTH_STATION, SOUTH_STATION, SINGLETRACK_EAST, SINGLETRACK_WEST, MIDDLE_DUBBLE_TRACK,CROSSING
  }

  private Map<SemaphoreName, Semaphore> semaphoreMap = new HashMap<>();
  private Map<List<Integer>, SensorName> sensorMap = new HashMap<>();
  private Map<SwitchName, List<Integer>> switchMap = new HashMap<>();

  public Lab1(int speed1, int speed2) {

    //All the switches.
    switchMap.put(SwitchName.NORTH_STATION_SWITCH, asList(17,7));
    switchMap.put(SwitchName.SOUTH_STATION_SWITCH,asList(3,11));
    switchMap.put(SwitchName.MIDDLE_SWITCH_EAST,asList(15,9));
    switchMap.put(SwitchName.MIDDLE_SWITCH_WEST,asList(4,9));

    //All the station sensors.
    sensorMap.put(asList(16,3), SensorName.NORTH_OF_NORTH_STATION);
    sensorMap.put(asList(16,5), SensorName.SOUTH_OF_NORTH_STATION);
    sensorMap.put(asList(16,11), SensorName.NORTH_OF_SOUTH_STATION);
    sensorMap.put(asList(16,13), SensorName.SOUTH_OF_SOUTH_STATION);

    //All the sensors surrounding the crossing.
    sensorMap.put(asList(9,5), SensorName.NORTH_OF_CROSSING);
    sensorMap.put(asList(6,7), SensorName.WEST_OF_CROSSING);
    sensorMap.put(asList(9,8), SensorName.SOUTH_OF_CROSSING);
    sensorMap.put(asList(10,7), SensorName.EAST_OF_CROSSING);

    //All the sensors surrounding the north station switch.
    sensorMap.put(asList(14,7), SensorName.WEST_OF_NORTH_STATION_SWITCH);
    sensorMap.put(asList(15,8), SensorName.SOUTHWEST_OF_NORTH_STATION_SWITCH);
    sensorMap.put(asList(18,7), SensorName.EAST_OF_NORTH_STATION_SWITCH);

    //All the sensors surrounding the south station switch.
    sensorMap.put(asList(2,11), SensorName.WEST_OF_SOUTH_STATION_SWITCH);
    sensorMap.put(asList(5,11), SensorName.EAST_OF_SOUTH_STATION_SWITCH);
    sensorMap.put(asList(4,13), SensorName.SOUTHEAST_OF_SOUTH_STATION_SWITCH);

    //All the sensors surrounding the switch east to the middle of the map.
    sensorMap.put(asList(16,9), SensorName.EAST_OF_MIDDLE_SWITCH_EAST);
    sensorMap.put(asList(12,9), SensorName.WEST_OF_MIDDLE_SWITCH_EAST);
    sensorMap.put(asList(13,10), SensorName.SOUTHWEST_OF_MIDDLE_SWITCH_EAST);

    //All the sensors surrounding the switch west to the middle of the map.
    sensorMap.put(asList(3,9), SensorName.WEST_OF_MIDDLE_SWITCH_WEST);
    sensorMap.put(asList(7,9), SensorName.EAST_OF_MIDDLE_SWITCH_WEST);
    sensorMap.put(asList(6,10), SensorName.SOUTHEAST_OF_MIDDLE_SWITCH_WEST);

    //All the semaphores.
    semaphoreMap.put(SemaphoreName.CROSSING, new Semaphore(1));
    semaphoreMap.put(SemaphoreName.NORTH_STATION, new Semaphore(1));
    semaphoreMap.put(SemaphoreName.SOUTH_STATION, new Semaphore(1));
    semaphoreMap.put(SemaphoreName.MIDDLE_DUBBLE_TRACK, new Semaphore(1));
    semaphoreMap.put(SemaphoreName.SINGLETRACK_EAST, new Semaphore(1));
    semaphoreMap.put(SemaphoreName.SINGLETRACK_WEST, new Semaphore(1));

    //The two trains to run on the map.
    Train train1 = new Train(1, speed1,TSimInterface.getInstance(), false);
    Train train2 = new Train(2, speed2, TSimInterface.getInstance(), true);

    // this is to ensure that the station train2 starts at is acquired even though it actually
    // never has entered the station but spontaneously spawned there.
    semaphoreMap.get(SemaphoreName.SOUTH_STATION).tryAcquire();

    train1.start();
    train2.start();
  }

  class Train extends Thread {
    private boolean headedNorth;   //used during sensor events to determine the way a train is headed. 
    private int velocity, maxVelocity;
    private int id;
    private TSimInterface tsi;

    Train(int id, int startVelocity, TSimInterface tsi, boolean direction) {
      this.id = id;
      this.maxVelocity = 19;
      setVelocity(startVelocity);
      this.tsi = tsi;
      this.headedNorth = direction;

      try {
        tsi.setSpeed(id,this.velocity);
      } catch (CommandException e) {
        e.printStackTrace();
        System.exit(1);
      }
    }

    private void setVelocity(int velocity) {
      if (velocity <= maxVelocity) {
        this.velocity = velocity;
      } else {
        this.velocity = maxVelocity;
      }
    }

    private void changeDirection() {
      velocity = -velocity;
      headedNorth = !headedNorth;
    }

    private void waitAtStation() {
      try {
        tsi.setSpeed(id,0);
        sleep(1000 + (20 * Math.abs(velocity)));
        changeDirection();
        tsi.setSpeed(id, velocity);
      } catch (CommandException e) {
        e.printStackTrace();
        System.exit(1);
      } catch (InterruptedException e) {
        e.printStackTrace();
        System.exit(1);
      }
    }

    private void setSwitch(SwitchName switchName, int direction) throws CommandException {
      tsi.setSwitch(switchMap.get(switchName).get(0),switchMap.get(switchName).get(1),direction);
    }

    //Stops the train and waits for the semaphore to be acquired before proceeding. 
    private void stopUntilPass(SemaphoreName semaphoreName) throws CommandException, InterruptedException {
      Semaphore semaphore = semaphoreMap.get(semaphoreName);
      tsi.setSpeed(id,0);
      semaphore.acquire();
      tsi.setSpeed(id, velocity);
    }

    private void releasePermit(SemaphoreName semaphoreName) {
      Semaphore semaphore = semaphoreMap.get(semaphoreName);
      if (semaphore.availablePermits() == 0) {
        semaphore.release();
      }
    }

    //Generalisation of the situation when the train is approaching a switch having to choose what way to go.
    private void choosePath(SemaphoreName semaphoreName, SwitchName switchName, int direction1, int direction2) throws CommandException{
      boolean permitAcquired = semaphoreMap.get(semaphoreName).tryAcquire();
      if (permitAcquired) {
        setSwitch(switchName, direction1);
      } else {
        setSwitch(switchName, direction2);
      }
    }

    //The main logic of the train class handling all the sensor events.
    private void manageSensorEvent(SensorEvent event) throws CommandException, InterruptedException {
      boolean isActive = event.getStatus() == SensorEvent.ACTIVE;
      SensorName sensorName = sensorMap.get(asList(event.getXpos(),event.getYpos()));
      if (isActive) {
        switch (sensorName) {
          case NORTH_OF_NORTH_STATION: case SOUTH_OF_NORTH_STATION:
            if (headedNorth)  waitAtStation();
            break;
          case NORTH_OF_SOUTH_STATION: case SOUTH_OF_SOUTH_STATION:
            if (!headedNorth) waitAtStation();
            break;
          case WEST_OF_CROSSING: case NORTH_OF_CROSSING:
            if (headedNorth) releasePermit(SemaphoreName.CROSSING);
            else stopUntilPass(SemaphoreName.CROSSING);
            break;
          case EAST_OF_CROSSING: case SOUTH_OF_CROSSING:
            if (headedNorth) stopUntilPass(SemaphoreName.CROSSING);
            else releasePermit(SemaphoreName.CROSSING);
            break;
          case EAST_OF_NORTH_STATION_SWITCH:
            if (!headedNorth) {
              releasePermit(SemaphoreName.NORTH_STATION);
            } else {
              choosePath(SemaphoreName.NORTH_STATION,SwitchName.NORTH_STATION_SWITCH,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
            }
            break;
          case WEST_OF_NORTH_STATION_SWITCH:
            if (!headedNorth) {
              stopUntilPass(SemaphoreName.SINGLETRACK_EAST);
              setSwitch(SwitchName.NORTH_STATION_SWITCH, TSimInterface.SWITCH_RIGHT);
            } else {
              releasePermit(SemaphoreName.SINGLETRACK_EAST);
            }
            break;
          case SOUTHWEST_OF_NORTH_STATION_SWITCH:
            if (!headedNorth) {
              stopUntilPass(SemaphoreName.SINGLETRACK_EAST);
              setSwitch(SwitchName.NORTH_STATION_SWITCH, TSimInterface.SWITCH_LEFT);
            } else {
              releasePermit(SemaphoreName.SINGLETRACK_EAST);
            }
            break;
          case WEST_OF_SOUTH_STATION_SWITCH:
            if (!headedNorth) {
              System.out.println(semaphoreMap.get(SemaphoreName.SOUTH_STATION).availablePermits());
              choosePath(SemaphoreName.SOUTH_STATION,SwitchName.SOUTH_STATION_SWITCH,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
            } else {
              releasePermit(SemaphoreName.SOUTH_STATION);
            }
            break;
          case EAST_OF_SOUTH_STATION_SWITCH:
            if (!headedNorth) {
              releasePermit(SemaphoreName.SINGLETRACK_WEST);
            } else {
              stopUntilPass(SemaphoreName.SINGLETRACK_WEST);
              setSwitch(SwitchName.SOUTH_STATION_SWITCH, TSimInterface.SWITCH_LEFT);
            }
            break;
          case SOUTHEAST_OF_SOUTH_STATION_SWITCH:
            if (!headedNorth) {
              releasePermit(SemaphoreName.SINGLETRACK_WEST);
            } else {
              stopUntilPass(SemaphoreName.SINGLETRACK_WEST);
              setSwitch(SwitchName.SOUTH_STATION_SWITCH, TSimInterface.SWITCH_RIGHT);
            }
            break;
          case WEST_OF_MIDDLE_SWITCH_WEST:
            if (headedNorth) {
              choosePath(SemaphoreName.MIDDLE_DUBBLE_TRACK,SwitchName.MIDDLE_SWITCH_WEST,TSimInterface.SWITCH_LEFT,TSimInterface.SWITCH_RIGHT);
            } else {
              releasePermit(SemaphoreName.MIDDLE_DUBBLE_TRACK);
            }
            break;
          case EAST_OF_MIDDLE_SWITCH_WEST:
            if (headedNorth) {
              releasePermit(SemaphoreName.SINGLETRACK_WEST);
            } else {
              stopUntilPass(SemaphoreName.SINGLETRACK_WEST);
              setSwitch(SwitchName.MIDDLE_SWITCH_WEST, TSimInterface.SWITCH_LEFT);
            }
            break;
          case SOUTHEAST_OF_MIDDLE_SWITCH_WEST:
            if (headedNorth) {
              releasePermit(SemaphoreName.SINGLETRACK_WEST);
            } else {
              stopUntilPass(SemaphoreName.SINGLETRACK_WEST);
              setSwitch(SwitchName.MIDDLE_SWITCH_WEST, TSimInterface.SWITCH_RIGHT);
            }
            break;
          case EAST_OF_MIDDLE_SWITCH_EAST:
            if (!headedNorth) {
              choosePath(SemaphoreName.MIDDLE_DUBBLE_TRACK,SwitchName.MIDDLE_SWITCH_EAST,TSimInterface.SWITCH_RIGHT,TSimInterface.SWITCH_LEFT);
            } else {
              releasePermit(SemaphoreName.MIDDLE_DUBBLE_TRACK);
            }
            break;
          case WEST_OF_MIDDLE_SWITCH_EAST:
            if (!headedNorth) {
              releasePermit(SemaphoreName.SINGLETRACK_EAST);
            } else {
              stopUntilPass(SemaphoreName.SINGLETRACK_EAST);
              setSwitch(SwitchName.MIDDLE_SWITCH_EAST, TSimInterface.SWITCH_RIGHT);
            }
            break;
          case SOUTHWEST_OF_MIDDLE_SWITCH_EAST:
            if (!headedNorth) {
              releasePermit(SemaphoreName.SINGLETRACK_EAST);
            } else {
              stopUntilPass(SemaphoreName.SINGLETRACK_EAST);
              setSwitch(SwitchName.MIDDLE_SWITCH_EAST, TSimInterface.SWITCH_LEFT);
            }
            break;
        }
      }
    }

    @Override
    public void run() {
      while (true) {
        try {
          tsi.setSpeed(id, this.velocity);
        } catch (CommandException e) {
          e.printStackTrace(); 
          System.exit(1);
        }
        while (!this.isInterrupted()) {
          try {
            manageSensorEvent(tsi.getSensor(id));
          } catch (CommandException e) {
            e.printStackTrace();
            System.exit(1);
          } catch (InterruptedException e) {
            e.printStackTrace();
            System.exit(1);
          }
        }
      }
    }
  }
}
