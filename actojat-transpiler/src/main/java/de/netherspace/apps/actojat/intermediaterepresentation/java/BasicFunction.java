package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;

public enum BasicFunction { // TODO: rename to 'BasicConstruct'

  PRINTLN("System.out.println"),
  RETURN("return"),
  FORLOOP("for(...)");

  @Getter
  private final String rawName;

  BasicFunction(String rawName) {
    this.rawName = rawName;
  }

}
