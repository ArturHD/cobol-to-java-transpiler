package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;

public class FunctionCall extends Statement {

  /**
   * The function's name (or 'identifier'),
   * e.g. 'doSomething' for this example:
   *   bla = doSomething();
   */
  @Getter
  @Setter
  private String name;

  @Getter
  private List<String> parameters;


  /**
   * Default constructor.
   */
  public FunctionCall() {
    super();
    this.parameters = new LinkedList<>();
  }


  /**
   * Auxiliary constructor.
   *
   * @param name the function call's name
   */
  public FunctionCall(String name) {
    super();
    this.name = name;
    this.parameters = new LinkedList<>();
  }

}
