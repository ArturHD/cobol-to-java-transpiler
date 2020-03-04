package my.base.pckg;

public class PerformTimes {
  public short n = 3;

  public void paragraph_MainProgram() {
    for (int _internalA2BE66F = 1;
        _internalA2BE66F <= n;
        _internalA2BE66F = (_internalA2BE66F + 1)) {
      System.out.println("Inline!");
    }
    for (int _internal509EB55 = 1;
        _internal509EB55 <= 15;
        _internal509EB55 = (_internal509EB55 + 1)) {
      paragraph_DisplayHelloWorld();
    }
    return;
  }

  public void paragraph_DisplayHelloWorld() {
    System.out.println("Hello");
    System.out.println("World!");
  }
}
