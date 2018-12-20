package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;

public class FunctionCall extends Statement {

  @Getter
  @Setter
  private String name;

  @Getter
  private List<String> parameters;

  public FunctionCall() {
    super();
    this.parameters = new LinkedList<>();
  }

}
