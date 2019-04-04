package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;
import lombok.Setter;

@Getter
public class Assignment extends Statement {

  private LeftHandSide lhs;

  @Setter
  private String rhs;

  public Assignment(LeftHandSide lhs) {
    this.lhs = lhs;
  }

}
