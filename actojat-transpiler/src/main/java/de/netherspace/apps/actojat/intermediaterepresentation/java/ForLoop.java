package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;

@Getter
public class ForLoop extends Loop {

  private Assignment loopVariable;
  private String loopCondition;
  private String loopIncrement;

  /**
   * Constructor.
   *
   * @param body          the loop body
   * @param loopVariable  the loop variable
   * @param loopCondition the loop condition
   * @param loopIncrement the loop increment statement
   */
  public ForLoop(Statement[] body, Assignment loopVariable,
                 String loopCondition, String loopIncrement) {
    super(body);
    this.loopVariable = loopVariable;
    this.loopCondition = loopCondition;
    this.loopIncrement = loopIncrement;
  }

}
