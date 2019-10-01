package my.base.pckg;

public class PerformTimes {
  public short n = 3;

  public void paragraph_MainProgram() {
    for (int _internal67B28F0 = 1; _internal67B28F0 <= n; _internal67B28F0++) {
      System.out.println("Inline!");
    }
    for (int _internal3434149 = 1; _internal3434149 <= 15; _internal3434149++) {
      paragraph_DisplayHelloWorld();
    }
    return;
  }

  public void paragraph_DisplayHelloWorld() {
    System.out.println("Hello");
    System.out.println("World!");
  }
}
