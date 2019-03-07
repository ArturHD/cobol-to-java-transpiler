package de.netherspace.apps.actojat.util;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * A generic pair: UxT.
 *
 * @param <T> the type of First
 * @param <U> the type of Second
 */
@Getter
@AllArgsConstructor
public class Pair<T, U> {

  private T first;
  private U second;

}
