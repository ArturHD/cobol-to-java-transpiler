package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;

public enum BasicFunction {

  PRINTLN("System.out.println");

  @Getter
  private final String rawName;

  BasicFunction(String rawName) {
    this.rawName = rawName;
  }

}
