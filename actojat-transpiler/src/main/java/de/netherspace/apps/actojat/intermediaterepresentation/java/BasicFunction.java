package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;

public enum BasicFunction { // TODO: rename to 'BasicConstruct'

  PRINTLN("System.out.println"),
  PRINT("System.out.print"),
  RETURN("return");

  @Getter
  private final String rawName;

  BasicFunction(String rawName) {
    this.rawName = rawName;
  }

}
