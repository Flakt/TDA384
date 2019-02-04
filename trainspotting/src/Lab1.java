import TSim.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

/*
  TODO: Refactor the implementation of the sensors
  TODO: Implement handling of SensorEvents
  TODO: Implement handling of switches
  TODO: Evaluate if current sensor placements can be improved

 */

/**
 * the class responsible for all the operations related to the running of a train instance
 */
public class Lab1 {

  /**
   * is used to identify the different switches
   */
  enum SwitchName {
    NORTH,SOUTH,PITSTOP_EAST,PITSTOP_WEST
  }

  /**
   * is used to identify the different sensors
   */
  enum SensorName {
    SOUTH_OF_NORTH_STATION,NORTH_OF_NORTH_STATION, CROSSING_WEST, CROSSING_NORTH, CROSSING_EAST, CROSSING_SOUTH,
    SOUTHWEST_OF_JUNCTION_NORTH, EAST_OF_JUNCTION_NORTH, WEST_OF_JUNCTION_NORTH, SOUTHEAST_OF_JUNCTION_SOUTH,
    EAST_OF_JUNCTION_SOUTH, WEST_OF_JUNCTION_SOUTH, NORTH_OF_SOUTH_STATION, SOUTH_OF_SOUTH_STATION, WEST_OF_PITSTOP_EAST,
    EAST_OF_PITSTOP_EAST, SOUTHWEST_OF_PITSTOP_EAST, WEST_OF_PITSTOP_WEST, EAST_OF_PITSTOP_WEST,
    SOUTHEAST_OF_PITSTOP_WEST
  }


  /**
   * is used to identify the different semaphores
   */
  enum SemaphoreName {
    NORTH,EAST,WEST,SOUTH,PITSTOP,CROSSING
  }

//
  private TSimInterface tsi;

  private List<Switch> switches = new ArrayList<>();

  private List<Sensor> sensors = new ArrayList<>();

  private List<RestrictedArea> semaphores = new ArrayList<>();


  public Lab1(int speed1, int speed2) {
    tsi = TSimInterface.getInstance();

    //All the switches
    switches.add(new Switch(17,7,SwitchName.NORTH));
    switches.add(new Switch(3,11,SwitchName.SOUTH));
    switches.add(new Switch(15,9,SwitchName.PITSTOP_EAST));
    switches.add(new Switch(4,9,SwitchName.PITSTOP_WEST));

    //All the sensors for the two stations
    sensors.add(new Sensor(16,3,SensorName.NORTH_OF_NORTH_STATION));
    sensors.add(new Sensor(16,5,SensorName.SOUTH_OF_NORTH_STATION));
    sensors.add(new Sensor(16,12,SensorName.NORTH_OF_SOUTH_STATION));
    sensors.add(new Sensor(16,13,SensorName.SOUTH_OF_SOUTH_STATION));

    //All the sensors for the crossing
    sensors.add(new Sensor(8,6,SensorName.CROSSING_NORTH));
    sensors.add(new Sensor(7,7,SensorName.CROSSING_WEST));
    sensors.add(new Sensor(9,8,SensorName.CROSSING_SOUTH));
    sensors.add(new Sensor(9,7,SensorName.CROSSING_EAST));

    //All the sensors for the northern junction
    sensors.add(new Sensor(16,7,SensorName.WEST_OF_JUNCTION_NORTH));
    sensors.add(new Sensor(16,8,SensorName.SOUTHWEST_OF_JUNCTION_NORTH));
    sensors.add(new Sensor(18,7,SensorName.EAST_OF_JUNCTION_NORTH));

    //All the sensors for the sothern junction
    sensors.add(new Sensor(2,11,SensorName.WEST_OF_JUNCTION_SOUTH));
    sensors.add(new Sensor(4,11,SensorName.EAST_OF_JUNCTION_SOUTH));
    sensors.add(new Sensor(3,12,SensorName.SOUTHEAST_OF_JUNCTION_SOUTH));

    //All the sensors for.....
    sensors.add(new Sensor(16,9,SensorName.EAST_OF_PITSTOP_EAST));
    sensors.add(new Sensor(14,9,SensorName.WEST_OF_PITSTOP_EAST));
    sensors.add(new Sensor(14,10,SensorName.SOUTHWEST_OF_PITSTOP_EAST));

    //All the sensors for.....
    sensors.add(new Sensor(3,8,SensorName.WEST_OF_PITSTOP_WEST));
    sensors.add(new Sensor(5,9,SensorName.EAST_OF_PITSTOP_WEST));
    sensors.add(new Sensor(5,10,SensorName.SOUTHEAST_OF_PITSTOP_WEST));

    //All the semaphores
    semaphores.add(new RestrictedArea(SemaphoreName.CROSSING,1));
    semaphores.add(new RestrictedArea(SemaphoreName.NORTH,1));
    semaphores.add(new RestrictedArea(SemaphoreName.SOUTH,1));
    semaphores.add(new RestrictedArea(SemaphoreName.PITSTOP,1));
    semaphores.add(new RestrictedArea(SemaphoreName.EAST,1));
    semaphores.add(new RestrictedArea(SemaphoreName.WEST,1));

    Train train1 = new Train(1,speed1,SensorName.NORTH_OF_NORTH_STATION,tsi,SemaphoreName.NORTH);
    Train train2 = new Train(2,speed2,SensorName.NORTH_OF_SOUTH_STATION,tsi,SemaphoreName.SOUTH);

    train1.start();
    train2.start();

  }

  /**
   * Class to represent the switches
   */
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

  /**
   * Class to represent the sensors
   */
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

  /**
   * class to representing a restricted area. 
   */
  class RestrictedArea {
    SemaphoreName semaphoreName;
    Semaphore semaphore;

    RestrictedArea(SemaphoreName semaphoreName, int permits) {
      this.semaphoreName = semaphoreName;
      this.semaphore = new Semaphore(permits);
    }
  }

  /**
   * Class to represent the trains
   */
  // TODO: Implement a representation of the train
  class Train extends Thread {
    boolean forward;
    private int velocity;
    int id;
    TSimInterface tsi;
    int maxVelocity;
    SemaphoreName lastSemaphore;
    SemaphoreName currentSemaphore;
    SensorName lastSensor;

    Train(int id, int startVelocity, SensorName startSensor, TSimInterface tsi, SemaphoreName semaphoreName) {
      this.id = id;
      this.forward = true;
      this.maxVelocity = 15;
      setVelocity(startVelocity);
      this.tsi = tsi;
      this.lastSensor = startSensor;
      this.currentSemaphore = semaphoreName;
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
      }
      else {
        this.velocity = maxVelocity;
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

    private void changeDirection() {
      forward = !forward;
    }

    private void waitAtStation() {
      try {
        activateBreak();
        sleep(1000 + (20 * velocity));
        changeDirection();
        goForward();
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

    private void setSwitch(SwitchName switchName, int direction) throws CommandException {
      Switch s = getSwitch(switchName);
      tsi.setSwitch(s.x,s.y,direction);
    }

    private Switch getSwitch(SwitchName switchName) {
      for (Switch s : switches) {
        if (s.switchName == switchName) {
          return s;
        }
      }
      return null;
    }

    private Semaphore getSemaphore(SemaphoreName name) {
      for (RestrictedArea s : semaphores) {
        if (s.semaphoreName == name) {
          return s.semaphore;
        }
      }
      return null;
    }

    private Sensor getSensor(int x, int y) {
      for (Sensor s : sensors) {
        if (s.x == x && s.y == y) {
          return s;
        }
      }
      return null;
    }

    private void stopUntilPass(SemaphoreName semaphoreName) throws CommandException, InterruptedException {
      Semaphore semaphore = getSemaphore(semaphoreName);
      activateBreak();
      semaphore.acquire();
      updateSemaphores(semaphoreName);
      goForward();
    }

    private void releasePermit(SemaphoreName semaphoreName) {
      Semaphore semaphore = getSemaphore(semaphoreName);
      if (semaphore.availablePermits() == 0) {
        semaphore.release();
        lastSemaphore = currentSemaphore;
      }
    }

    private boolean semaphoreHasPermits(SemaphoreName semaphoreName) {
      Semaphore semaphore = getSemaphore(semaphoreName);
      boolean hasPermit = semaphore.tryAcquire();
      updateSemaphores(semaphoreName);
      return hasPermit;
    }

    private void updateSemaphores(SemaphoreName semaphoreName) {
      if (semaphoreName != SemaphoreName.CROSSING) {
        lastSemaphore = currentSemaphore;
        currentSemaphore = semaphoreName;
      }
    }

    private void manageSensorEvent(SensorEvent event) throws CommandException, InterruptedException {
      boolean isActive = event.getStatus() == SensorEvent.ACTIVE;
      System.out.println("I am doing my job");
      SensorName sensorName = getSensor(event.getXpos(), event.getYpos()).sensorName;
      if (isActive) {
        switch (sensorName) {
          case NORTH_OF_NORTH_STATION:
            if (lastSensor == SensorName.CROSSING_WEST) {
              waitAtStation();
            }
            break;
          case SOUTH_OF_NORTH_STATION:
            if (lastSensor == SensorName.CROSSING_NORTH) {
              waitAtStation();
            }
            break;
          case NORTH_OF_SOUTH_STATION:
            if (lastSensor == SensorName.EAST_OF_JUNCTION_SOUTH) {
              waitAtStation();
            }
            break;
          case SOUTH_OF_SOUTH_STATION:
            if (lastSensor == SensorName.SOUTHEAST_OF_JUNCTION_SOUTH) {
              waitAtStation();
            }
            break;

          case CROSSING_WEST:
            if (lastSensor == SensorName.CROSSING_EAST) {
              releasePermit(SemaphoreName.CROSSING);
            }
            else {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            break;
          case CROSSING_EAST:
            if (lastSensor == SensorName.WEST_OF_JUNCTION_NORTH) {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            else {
              releasePermit(SemaphoreName.CROSSING);
            }
            break;
          case CROSSING_NORTH:
            if (lastSensor == SensorName.CROSSING_SOUTH) {
              releasePermit(SemaphoreName.CROSSING);
            }
            else {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            break;
          case CROSSING_SOUTH:
            if (lastSensor == SensorName.EAST_OF_JUNCTION_NORTH) {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            else {
              releasePermit(SemaphoreName.CROSSING);
            }
            break;

          case EAST_OF_JUNCTION_NORTH:
            if (lastSensor == SensorName.WEST_OF_JUNCTION_NORTH || lastSensor == SensorName.SOUTHWEST_OF_JUNCTION_NORTH) {
              releasePermit(SemaphoreName.NORTH);
            }
            else if (lastSensor == SensorName.EAST_OF_PITSTOP_EAST) {
              if (semaphoreHasPermits(SemaphoreName.NORTH)) {
                setSwitch(SwitchName.NORTH, TSimInterface.SWITCH_RIGHT);
                goForward();
              }
              else {
                setSwitch(SwitchName.NORTH, TSimInterface.SWITCH_LEFT);
              }
            }
            break;
          case WEST_OF_JUNCTION_NORTH:
            if (lastSensor == SensorName.CROSSING_EAST) {
              setSwitch(SwitchName.NORTH, TSimInterface.SWITCH_RIGHT);
              stopUntilPass(SemaphoreName.NORTH);
            }
            else {
              releasePermit(SemaphoreName.NORTH);
            }
            break;
          case SOUTHWEST_OF_JUNCTION_NORTH:
            if (lastSensor == SensorName.CROSSING_SOUTH) {
              setSwitch(SwitchName.NORTH, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.NORTH);
            }
            else {
              releasePermit(SemaphoreName.NORTH);
            }

          case WEST_OF_JUNCTION_SOUTH:
            if (lastSensor == SensorName.WEST_OF_PITSTOP_WEST) {
              if (semaphoreHasPermits(SemaphoreName.SOUTH)) {
                setSwitch(SwitchName.SOUTH, TSimInterface.SWITCH_LEFT);
                goForward();
              }
              else {
                setSwitch(SwitchName.SOUTH, TSimInterface.SWITCH_RIGHT);
              }
            }
            else {
              releasePermit(SemaphoreName.SOUTH);
            }
            break;
          case EAST_OF_JUNCTION_SOUTH:
            if (lastSensor == SensorName.WEST_OF_JUNCTION_SOUTH) {
              releasePermit(SemaphoreName.SOUTH);
            }
            else {
              setSwitch(SwitchName.SOUTH, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.SOUTH);
            }
            break;
          case SOUTHEAST_OF_JUNCTION_SOUTH:
            if (lastSensor == SensorName.WEST_OF_JUNCTION_SOUTH) {
              releasePermit(SemaphoreName.SOUTH);
            }
            else {
              setSwitch(SwitchName.SOUTH, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.SOUTH);
            }
            break;

          case WEST_OF_PITSTOP_WEST:
            if (lastSensor == SensorName.WEST_OF_JUNCTION_SOUTH) {
              if (semaphoreHasPermits(SemaphoreName.PITSTOP)) {
                setSwitch(SwitchName.PITSTOP_WEST, TSimInterface.SWITCH_LEFT);
                goForward();
              }
              else {
                setSwitch(SwitchName.PITSTOP_WEST, TSimInterface.SWITCH_RIGHT);
              }
            }
            else {
              releasePermit(SemaphoreName.PITSTOP);
            }
            break;
          case EAST_OF_PITSTOP_WEST:
            if (lastSensor == SensorName.WEST_OF_PITSTOP_WEST) {
              releasePermit(SemaphoreName.WEST);
            }
            else {
              setSwitch(SwitchName.PITSTOP_WEST, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.WEST);
            }
            break;
          case SOUTHEAST_OF_PITSTOP_WEST:
            if (lastSensor == SensorName.WEST_OF_PITSTOP_WEST) {
              releasePermit(SemaphoreName.WEST);
            }
            else {
              setSwitch(SwitchName.PITSTOP_WEST, TSimInterface.SWITCH_RIGHT);
              stopUntilPass(SemaphoreName.WEST);
            }
            break;

          case EAST_OF_PITSTOP_EAST:
            if (lastSensor == SensorName.WEST_OF_JUNCTION_NORTH) {
              if (semaphoreHasPermits(SemaphoreName.PITSTOP)) {
                setSwitch(SwitchName.PITSTOP_EAST, TSimInterface.SWITCH_LEFT);
                goForward();
              }
              else {
                setSwitch(SwitchName.PITSTOP_EAST, TSimInterface.SWITCH_RIGHT);
              }
            }
            else {
              releasePermit(SemaphoreName.PITSTOP);
            }
            break;
          case WEST_OF_PITSTOP_EAST:
            if (lastSensor == SensorName.EAST_OF_PITSTOP_EAST) {
              releasePermit(SemaphoreName.EAST);
            }
            else {
              setSwitch(SwitchName.PITSTOP_EAST, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.EAST);
            }
            break;
          case SOUTHWEST_OF_PITSTOP_EAST:
            if (lastSensor == SensorName.EAST_OF_PITSTOP_EAST) {
              releasePermit(SemaphoreName.EAST);
            }
            else {
              setSwitch(SwitchName.PITSTOP_EAST, TSimInterface.SWITCH_RIGHT);
              stopUntilPass(SemaphoreName.EAST);
            }
            break;
        }
        lastSensor = sensorName;
      }
    }


    @Override
    public void run() {
      while (true) {
        try {
          tsi.setSpeed(id, this.velocity);

        } catch (CommandException e) {
          e.printStackTrace();    // or only e.getMessage() for the error
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
