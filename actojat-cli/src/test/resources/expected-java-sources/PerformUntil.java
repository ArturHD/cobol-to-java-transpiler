package my.base.pckg;

public class PerformUntil {
  public int MyVar = 1;
  public int VeryVariable = 1;

  public void paragraph_MainProgram() {
    while (!(MyVar == 13)) {
      System.out.println("Inline!");
    }
    while (!(VeryVariable == 8)) {
      paragraph_DisplayRockOn();
    }
    System.out.println("Done!");
    return;
  }

  public void paragraph_DisplayRockOn() {
    System.out.println("Rock");
    System.out.println("on!");
  }
}
