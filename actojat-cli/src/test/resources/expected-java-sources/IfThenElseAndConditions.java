package my.base.pckg;

public class IfThenElseAndConditions {
  public short n = 5;
  public short a = 25;
  public short b = 15;
  public short c = 100;

  public void paragraph_MainProgram() {
    if (!(n > 20)) {
      System.out.println("correct");
    } else {
      System.out.println("notcorrect");
    }
    if (n < 10) {
      System.out.println("yeah");
    }
    if (!(n < 10)) {
      System.out.println("notlezz");
    }
    if (!(n == 775)) {
      System.out.println("noteq");
    } else {
      System.out.println("eq");
    }
    if (n <= 33) {
      System.out.println("lteq33");
    }
    if (n <= 77) {
      System.out.println("ltort77");
    }
    if (a < (b + (c / 2))) {
      System.out.println("correct");
    }
    return;
  }
}
