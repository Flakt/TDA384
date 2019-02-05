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
    NORTH_STATION_SWITCH, SOUTH_STATION_SWITCH, MIDDLE_SWITCH_EAST, MIDDLE_SWITCH_WEST
  }

  /**
   * is used to identify the different sensors
   */
  enum SensorName {
    SOUTH_OF_NORTH_STATION,NORTH_OF_NORTH_STATION, WEST_OF_CROSSING, NORTH_OF_CROSSING, EAST_OF_CROSSING, SOUTH_OF_CROSSING,
    SOUTHWEST_OF_NORTH_STATION_SWITCH, EAST_OF_NORTH_STATION_SWITCH, WEST_OF_NORTH_STATION_SWITCH, SOUTHEAST_OF_SOUTH_STATION_SWITCH,
    EAST_OF_SOUTH_STATION_SWITCH, WEST_OF_SOUTH_STATION_SWITCH, NORTH_OF_SOUTH_STATION, SOUTH_OF_SOUTH_STATION, WEST_OF_MIDDLE_SWITCH_EAST,
    EAST_OF_MIDDLE_SWITCH_EAST, SOUTHWEST_OF_MIDDLE_SWITCH_EAST, WEST_OF_MIDDLE_SWITCH_WEST, EAST_OF_MIDDLE_SWITCH_WEST,
    SOUTHEAST_OF_MIDDLE_SWITCH_WEST
  }


  /**
   * is used to identify the different semaphores
   */
  enum SemaphoreName {
    NORTH_STATION, SINGLETRACK_EAST, SINGLETRACK_WEST, SOUTH_STATION, MIDDLE_DUBBLE_TRACK,CROSSING
  }

//
  private TSimInterface tsi;

  private List<Switch> switches = new ArrayList<>();

  private List<Sensor> sensors = new ArrayList<>();

  private List<SemaphoreArea> semaphores = new ArrayList<>();


  public Lab1(int speed1, int speed2) {
    tsi = TSimInterface.getInstance();

    //All the switches.
    switches.add(new Switch(17,7,SwitchName.NORTH_STATION_SWITCH));
    switches.add(new Switch(3,11,SwitchName.SOUTH_STATION_SWITCH));
    switches.add(new Switch(15,9,SwitchName.MIDDLE_SWITCH_EAST));
    switches.add(new Switch(4,9,SwitchName.MIDDLE_SWITCH_WEST));

    //All the stations.
    sensors.add(new Sensor(16,3,SensorName.NORTH_OF_NORTH_STATION));
    sensors.add(new Sensor(16,5,SensorName.SOUTH_OF_NORTH_STATION));
    sensors.add(new Sensor(16,12,SensorName.NORTH_OF_SOUTH_STATION));
    sensors.add(new Sensor(16,13,SensorName.SOUTH_OF_SOUTH_STATION));

    //All the sensors surrounding the crossing.
    sensors.add(new Sensor(8,6,SensorName.NORTH_OF_CROSSING));
    sensors.add(new Sensor(7,7,SensorName.WEST_OF_CROSSING));
    sensors.add(new Sensor(9,8,SensorName.SOUTH_OF_CROSSING));
    sensors.add(new Sensor(9,7,SensorName.EAST_OF_CROSSING));

    //All the sensors surrounding the north station switch.
    sensors.add(new Sensor(16,7,SensorName.WEST_OF_NORTH_STATION_SWITCH));
    sensors.add(new Sensor(16,8,SensorName.SOUTHWEST_OF_NORTH_STATION_SWITCH));
    sensors.add(new Sensor(18,7,SensorName.EAST_OF_NORTH_STATION_SWITCH));

    //All the sensors surrounding the south station switch.
    sensors.add(new Sensor(2,11,SensorName.WEST_OF_SOUTH_STATION_SWITCH));
    sensors.add(new Sensor(4,11,SensorName.EAST_OF_SOUTH_STATION_SWITCH));
    sensors.add(new Sensor(3,12,SensorName.SOUTHEAST_OF_SOUTH_STATION_SWITCH));

    //All the sensors surrounding the switch east to the middle of the map.
    sensors.add(new Sensor(16,9,SensorName.EAST_OF_MIDDLE_SWITCH_EAST));
    sensors.add(new Sensor(14,9,SensorName.WEST_OF_MIDDLE_SWITCH_EAST));
    sensors.add(new Sensor(14,10,SensorName.SOUTHWEST_OF_MIDDLE_SWITCH_EAST));

    //All the sensors surrounding the switch west to the middle of the map.
    sensors.add(new Sensor(3,8,SensorName.WEST_OF_MIDDLE_SWITCH_WEST));
    sensors.add(new Sensor(5,9,SensorName.EAST_OF_MIDDLE_SWITCH_WEST));
    sensors.add(new Sensor(5,10,SensorName.SOUTHEAST_OF_MIDDLE_SWITCH_WEST));

    //All the semaphores.
    semaphores.add(new SemaphoreArea(SemaphoreName.CROSSING,1));
    semaphores.add(new SemaphoreArea(SemaphoreName.NORTH_STATION,1));
    semaphores.add(new SemaphoreArea(SemaphoreName.SOUTH_STATION,1));
    semaphores.add(new SemaphoreArea(SemaphoreName.MIDDLE_DUBBLE_TRACK,1));
    semaphores.add(new SemaphoreArea(SemaphoreName.SINGLETRACK_EAST,1));
    semaphores.add(new SemaphoreArea(SemaphoreName.SINGLETRACK_WEST,1));

    //The two trains to run on the map.
    Train train1 = new Train(1,speed1,SensorName.NORTH_OF_NORTH_STATION,tsi,SemaphoreName.NORTH_STATION);
    Train train2 = new Train(2,speed2,SensorName.NORTH_OF_SOUTH_STATION,tsi,SemaphoreName.SOUTH_STATION);

    train1.start();
    train2.start();

  }

  /**
   * Class to represent the different switches.
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
   * Class to represent the different sensors.
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
   * Class to represent the different areas limited by semaphores.
   */
  class SemaphoreArea {
    SemaphoreName semaphoreName;
    Semaphore semaphore;

    SemaphoreArea(SemaphoreName semaphoreName, int permits) {
      this.semaphoreName = semaphoreName;
      this.semaphore = new Semaphore(permits);
    }
  }

  /**
   * Class containing all the behavior and logic necessary for the function of a given train.
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
      for (SemaphoreArea s : semaphores) {
        if (s.semaphoreName == name) {
          return s.semaphore;
        }
      }
      return null;
    }

    /**
     * Gets the sensor corresponding to the given coordinates.
     *
     * @param x   x coordinate
     * @param y   y coordinate
     * @return    the sensor corresponding to the coordinates or null if none are found.
     */
    private Sensor getSensor(int x, int y) {
      for (Sensor s : sensors) {
        if (s.x == x && s.y == y) {
          return s;
        }
      }
      return null;
    }

    /**
     * Stops the train until it is able to acquire the corresponding semaphore.
     *
     * @param semaphoreName         The name corresponding to the semaphore that is to be acquired.
     * @throws CommandException
     * @throws InterruptedException
     */
    private void stopUntilPass(SemaphoreName semaphoreName) throws CommandException, InterruptedException {
      Semaphore semaphore = getSemaphore(semaphoreName);
      activateBreak();
      semaphore.acquire();
      updateSemaphores(semaphoreName);
      goForward();
    }

    /**
     * Releases the permit of a given semaphore.
     *
     * @param semaphoreName   The name of the semaphore who's permit is to be released.
     */
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

    /**
     * Sets the acquired semaphore to the current one and saves the old one as the last semaphore.
     *
     * @param semaphoreName   The newly acquired semaphore.
     */
    private void updateSemaphores(SemaphoreName semaphoreName) {
      if (semaphoreName != SemaphoreName.CROSSING) {  //TODO: why compare to the crossing???
        lastSemaphore = currentSemaphore;
        currentSemaphore = semaphoreName;
      }
    }

    /**
     * Handels sensor events for the given train depending what sensor has be triggered.
     *
     * @param event   The given sensor event to handel.
     * @throws CommandException
     * @throws InterruptedException
     */
    private void manageSensorEvent(SensorEvent event) throws CommandException, InterruptedException {
      boolean isActive = event.getStatus() == SensorEvent.ACTIVE;
      System.out.println("I am doing my job");
      SensorName sensorName = getSensor(event.getXpos(), event.getYpos()).sensorName;
      if (isActive) {
        switch (sensorName) {
          case NORTH_OF_NORTH_STATION:
            if (lastSensor == SensorName.WEST_OF_CROSSING) {
              waitAtStation();
            }
            break;
          case SOUTH_OF_NORTH_STATION:
            if (lastSensor == SensorName.NORTH_OF_CROSSING) {
              waitAtStation();
            }
            break;
          case NORTH_OF_SOUTH_STATION:
            if (lastSensor == SensorName.EAST_OF_SOUTH_STATION_SWITCH) {
              waitAtStation();
            }
            break;
          case SOUTH_OF_SOUTH_STATION:
            if (lastSensor == SensorName.SOUTHEAST_OF_SOUTH_STATION_SWITCH) {
              waitAtStation();
            }
            break;

          case WEST_OF_CROSSING:
            if (lastSensor == SensorName.EAST_OF_CROSSING) {
              releasePermit(SemaphoreName.CROSSING);
            }
            else {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            break;
          case EAST_OF_CROSSING:
            if (lastSensor == SensorName.WEST_OF_NORTH_STATION_SWITCH) {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            else {
              releasePermit(SemaphoreName.CROSSING);
            }
            break;
          case NORTH_OF_CROSSING:
            if (lastSensor == SensorName.SOUTH_OF_CROSSING) {
              releasePermit(SemaphoreName.CROSSING);
            }
            else {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            break;
          case SOUTH_OF_CROSSING:
            if (lastSensor == SensorName.EAST_OF_NORTH_STATION_SWITCH) {
              stopUntilPass(SemaphoreName.CROSSING);
            }
            else {
              releasePermit(SemaphoreName.CROSSING);
            }
            break;

          case EAST_OF_NORTH_STATION_SWITCH:
            if (lastSensor == SensorName.WEST_OF_NORTH_STATION_SWITCH || lastSensor == SensorName.SOUTHWEST_OF_NORTH_STATION_SWITCH) {
              releasePermit(SemaphoreName.NORTH_STATION);
            }
            else if (lastSensor == SensorName.EAST_OF_MIDDLE_SWITCH_EAST) {
              if (semaphoreHasPermits(SemaphoreName.NORTH_STATION)) {
                setSwitch(SwitchName.NORTH_STATION_SWITCH, TSimInterface.SWITCH_RIGHT);
                goForward();
              }
              else {
                setSwitch(SwitchName.NORTH_STATION_SWITCH, TSimInterface.SWITCH_LEFT);
              }
            }
            break;
          case WEST_OF_NORTH_STATION_SWITCH:
            if (lastSensor == SensorName.EAST_OF_CROSSING) {
              setSwitch(SwitchName.NORTH_STATION_SWITCH, TSimInterface.SWITCH_RIGHT);
              stopUntilPass(SemaphoreName.NORTH_STATION);
            }
            else {
              releasePermit(SemaphoreName.NORTH_STATION);
            }
            break;
          case SOUTHWEST_OF_NORTH_STATION_SWITCH:
            if (lastSensor == SensorName.SOUTH_OF_CROSSING) {
              setSwitch(SwitchName.NORTH_STATION_SWITCH, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.NORTH_STATION);
            }
            else {
              releasePermit(SemaphoreName.NORTH_STATION);
            }

          case WEST_OF_SOUTH_STATION_SWITCH:
            if (lastSensor == SensorName.WEST_OF_MIDDLE_SWITCH_WEST) {
              if (semaphoreHasPermits(SemaphoreName.SOUTH_STATION)) {
                setSwitch(SwitchName.SOUTH_STATION_SWITCH, TSimInterface.SWITCH_LEFT);
                goForward();
              }
              else {
                setSwitch(SwitchName.SOUTH_STATION_SWITCH, TSimInterface.SWITCH_RIGHT);
              }
            }
            else {
              releasePermit(SemaphoreName.SOUTH_STATION);
            }
            break;
          case EAST_OF_SOUTH_STATION_SWITCH:
            if (lastSensor == SensorName.WEST_OF_SOUTH_STATION_SWITCH) {
              releasePermit(SemaphoreName.SOUTH_STATION);
            }
            else {
              setSwitch(SwitchName.SOUTH_STATION_SWITCH, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.SOUTH_STATION);
            }
            break;
          case SOUTHEAST_OF_SOUTH_STATION_SWITCH:
            if (lastSensor == SensorName.WEST_OF_SOUTH_STATION_SWITCH) {
              releasePermit(SemaphoreName.SOUTH_STATION);
            }
            else {
              setSwitch(SwitchName.SOUTH_STATION_SWITCH, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.SOUTH_STATION);
            }
            break;

          case WEST_OF_MIDDLE_SWITCH_WEST:
            if (lastSensor == SensorName.WEST_OF_SOUTH_STATION_SWITCH) {
              if (semaphoreHasPermits(SemaphoreName.MIDDLE_DUBBLE_TRACK)) {
                setSwitch(SwitchName.MIDDLE_SWITCH_WEST, TSimInterface.SWITCH_LEFT);
                goForward();
              }
              else {
                setSwitch(SwitchName.MIDDLE_SWITCH_WEST, TSimInterface.SWITCH_RIGHT);
              }
            }
            else {
              releasePermit(SemaphoreName.MIDDLE_DUBBLE_TRACK);
            }
            break;
          case EAST_OF_MIDDLE_SWITCH_WEST:
            if (lastSensor == SensorName.WEST_OF_MIDDLE_SWITCH_WEST) {
              releasePermit(SemaphoreName.SINGLETRACK_WEST);
            }
            else {
              setSwitch(SwitchName.MIDDLE_SWITCH_WEST, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.SINGLETRACK_WEST);
            }
            break;
          case SOUTHEAST_OF_MIDDLE_SWITCH_WEST:
            if (lastSensor == SensorName.WEST_OF_MIDDLE_SWITCH_WEST) {
              releasePermit(SemaphoreName.SINGLETRACK_WEST);
            }
            else {
              setSwitch(SwitchName.MIDDLE_SWITCH_WEST, TSimInterface.SWITCH_RIGHT);
              stopUntilPass(SemaphoreName.SINGLETRACK_WEST);
            }
            break;

          case EAST_OF_MIDDLE_SWITCH_EAST:
            if (lastSensor == SensorName.WEST_OF_NORTH_STATION_SWITCH) {
              if (semaphoreHasPermits(SemaphoreName.MIDDLE_DUBBLE_TRACK)) {
                setSwitch(SwitchName.MIDDLE_SWITCH_EAST, TSimInterface.SWITCH_LEFT);
                goForward();
              }
              else {
                setSwitch(SwitchName.MIDDLE_SWITCH_EAST, TSimInterface.SWITCH_RIGHT);
              }
            }
            else {
              releasePermit(SemaphoreName.MIDDLE_DUBBLE_TRACK);
            }
            break;
          case WEST_OF_MIDDLE_SWITCH_EAST:
            if (lastSensor == SensorName.EAST_OF_MIDDLE_SWITCH_EAST) {
              releasePermit(SemaphoreName.SINGLETRACK_EAST);
            }
            else {
              setSwitch(SwitchName.MIDDLE_SWITCH_EAST, TSimInterface.SWITCH_LEFT);
              stopUntilPass(SemaphoreName.SINGLETRACK_EAST);
            }
            break;
          case SOUTHWEST_OF_MIDDLE_SWITCH_EAST:
            if (lastSensor == SensorName.EAST_OF_MIDDLE_SWITCH_EAST) {
              releasePermit(SemaphoreName.SINGLETRACK_EAST);
            }
            else {
              setSwitch(SwitchName.MIDDLE_SWITCH_EAST, TSimInterface.SWITCH_RIGHT);
              stopUntilPass(SemaphoreName.SINGLETRACK_EAST);
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
