package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;

@Getter
public class ForLoop extends Loop {

  private String loopCounter;

  public ForLoop(String loopCounter, Statement[] body) {
    super(body);
    this.loopCounter = loopCounter;
  }

}
