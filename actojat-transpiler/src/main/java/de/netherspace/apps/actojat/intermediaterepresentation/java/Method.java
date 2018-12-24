package de.netherspace.apps.actojat.intermediaterepresentation.java;

import lombok.Getter;
import lombok.Setter;

import java.util.LinkedList;
import java.util.List;

public class Method extends JavaLanguageConstruct {

  @Getter
  @Setter
  private String name;

  @Getter
  private List<Statement> statements;

  @Getter
  private List<Argument> arguments;


  /**
   * Default constructor.
   */
  public Method() {
    super();
    this.statements = new LinkedList<>();
    this.arguments = new LinkedList<>();
  }


  /**
   * Auxiliary constructor.
   *
   * @param name the method's name
   */
  public Method(String name) {
    super();
    this.name = name;
    this.statements = new LinkedList<>();
    this.arguments = new LinkedList<>();
  }

}
