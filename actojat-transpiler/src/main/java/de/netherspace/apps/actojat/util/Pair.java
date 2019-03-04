package de.netherspace.apps.actojat.util;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * A generic pair: UxT.
 *
 * @param <T> the type of T
 * @param <U> the type of U
 */
@Getter
@AllArgsConstructor
public class Pair<T, U> {

  private T t;
  private U u;

}
