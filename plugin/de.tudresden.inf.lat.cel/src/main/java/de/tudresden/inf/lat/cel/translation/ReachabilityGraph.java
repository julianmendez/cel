/*
 * Copyright 2009 Julian Mendez
 *
 *
 * This file is part of CEL Plug-in.
 *
 * CEL Plug-in is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * CEL Plug-in is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with CEL Plug-in.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package de.tudresden.inf.lat.cel.translation;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * A reachability graph is a directed graph where for vertices 's' and 't',
 * there is an arc from 's' to 't' if and only if 't' is reachable from 's'.
 */
public class ReachabilityGraph<T> {

	/** This map stores for each vertex s which vertices are reachable from s. */
	private Map<T, Set<T>> reachableMap = new HashMap<T, Set<T>>();

	public ReachabilityGraph() {
	}

	/**
	 * Defines that the vertices in set dest are reachable from orig.
	 * 
	 * @param orig
	 * @param dest
	 */
	public void addReachable(T orig, Set<T> dest) {
		addVertices(Collections.singleton(orig));
		addVertices(dest);

		// consider the new vertices
		Set<T> allReachableVertices = this.reachableMap.get(orig);
		allReachableVertices.addAll(dest);

		// include all the reachable vertices according to the new vertices
		for (T elem : dest) {
			allReachableVertices.addAll(this.reachableMap.get(elem));
		}

		// update the reachable vertices for the vertices such that orig is
		// reachable
		for (T elem : getVertices()) {
			Set<T> accVertices = this.reachableMap.get(elem);
			if (accVertices.contains(orig)) {
				accVertices.addAll(allReachableVertices);
			}
		}
	}

	/**
	 * Defines that dest is reachable from orig.
	 * 
	 * @param orig
	 * @param dest
	 */
	public void addReachable(T orig, T dest) {
		addReachable(orig, Collections.singleton(dest));
	}

	/**
	 * Add vertices to the graph.
	 * 
	 * @param newVertices
	 *            vertices to be added.
	 */
	public void addVertices(Set<T> newVertices) {
		for (T elem : newVertices) {
			if (this.reachableMap.get(elem) == null) {
				this.reachableMap.put(elem, new HashSet<T>());
			}
		}
	}

	public Set<T> getDirectSuccessors(T vertex) {
		Set<T> ret = new HashSet<T>();
		Map<T, Set<T>> map = makeMapWithoutEquivalentVertices();
		Set<T> connectedVertices = new HashSet<T>();
		connectedVertices.addAll(map.get(vertex));
		Map<T, Integer> count = new HashMap<T, Integer>();
		for (T elem : connectedVertices) {
			count.put(elem, map.get(elem).size());
		}

		boolean changed = true;
		while (changed) {
			changed = false;
			T otherVertex = null;
			Iterator<T> it = connectedVertices.iterator();
			while (otherVertex == null && it.hasNext()) {
				T elem = it.next();
				if (count.get(elem) == 0) {
					otherVertex = elem;
				}
			}
			if (otherVertex != null) {
				changed = true;
				boolean directlyConnected = true;
				for (T elem : connectedVertices) {
					if (map.get(elem).contains(otherVertex)) {
						count.put(elem, count.get(elem) - 1);
						directlyConnected = false;
					}
				}
				if (directlyConnected) {
					ret.add(otherVertex);
				}
				connectedVertices.remove(otherVertex);
			}
		}
		return ret;
	}

	/**
	 * Two vertices s,t are called "equivalent" if s can be reached from t and
	 * vice versa.
	 * 
	 * @return equivalent classes of vertices
	 */
	public Set<Set<T>> getEquivalentClasses() {
		Set<Set<T>> ret = new HashSet<Set<T>>();
		Set<T> visited = new HashSet<T>();
		for (T elem : getVertices()) {
			if (!visited.contains(elem)) {
				Set<T> equivVertices = getEquivalentVertices(elem);
				ret.add(equivVertices);
				visited.addAll(equivVertices);
			}
		}
		return ret;
	}

	/**
	 * 
	 * @see ReachabilityGraph#getEquivalentClasses()
	 * @param orig
	 * @return the equivalent vertices of a particular vertex
	 */
	public Set<T> getEquivalentVertices(T orig) {
		Set<T> ret = new HashSet<T>();
		ret.add(orig);
		for (T otherElem : this.reachableMap.get(orig)) {
			if (this.reachableMap.get(otherElem).contains(orig)) {
				ret.add(otherElem);
			}
		}
		return ret;
	}

	public Set<T> getReachableVertices(T orig) {
		return Collections.unmodifiableSet(this.reachableMap.get(orig));
	}

	public Set<T> getVertices() {
		return Collections.unmodifiableSet(this.reachableMap.keySet());
	}

	protected Map<T, Set<T>> makeMapWithoutEquivalentVertices() {
		Map<T, Set<T>> ret = new HashMap<T, Set<T>>();
		for (T elem : getVertices()) {
			Set<T> otherSet = new HashSet<T>();
			otherSet.addAll(this.reachableMap.get(elem));
			if (otherSet.contains(elem)) {
				otherSet.removeAll(getEquivalentVertices(elem));
			}
			ret.put(elem, otherSet);
		}
		return ret;
	}

	public String toString() {
		StringBuffer ret = new StringBuffer();
		for (T vertex : getVertices()) {
			ret.append("(" + vertex.toString() + ") > (");
			for (T otherVertex : this.reachableMap.get(vertex)) {
				ret.append(" (" + otherVertex.toString() + ") ");
			}
			ret.append(")\n");
		}
		return ret.toString();
	}
}
