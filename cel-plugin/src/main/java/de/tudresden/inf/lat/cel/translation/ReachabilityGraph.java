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
import java.util.Optional;
import java.util.Set;

import de.tudresden.inf.lat.util.map.OptMap;
import de.tudresden.inf.lat.util.map.OptMapImpl;

/**
 * A reachability graph is a directed graph where for vertices 's' and 't',
 * there is an arc from 's' to 't' if and only if 't' is reachable from 's'.
 * 
 * @param <T>
 *            vertex type
 */
public class ReachabilityGraph<T> {

	/**
	 * This map stores for each vertex s which vertices are reachable from s.
	 */
	private final OptMap<T, Set<T>> reachableMap = new OptMapImpl<>(new HashMap<>());

	public ReachabilityGraph() {
	}

	/**
	 * Defines that the vertices in set dest are reachable from orig.
	 *
	 * @param orig
	 *            orig
	 * @param dest
	 *            dest
	 */
	public void addReachable(T orig, Set<T> dest) {
		addVertices(Collections.singleton(orig));
		addVertices(dest);

		// consider the new vertices
		Optional<Set<T>> optAllReachableVertices = this.reachableMap.get(orig);
		assert (optAllReachableVertices.isPresent());
		Set<T> allReachableVertices = optAllReachableVertices.get();
		allReachableVertices.addAll(dest);

		// include all the reachable vertices according to the new vertices
		for (T elem : dest) {
			Optional<Set<T>> optVertices = this.reachableMap.get(elem);
			assert (optVertices.isPresent());
			allReachableVertices.addAll(optVertices.get());
		}

		// update the reachable vertices for the vertices such that orig is
		// reachable
		for (T elem : getVertices()) {
			Optional<Set<T>> optAccVertices = this.reachableMap.get(elem);
			assert (optAccVertices.isPresent());
			Set<T> accVertices = optAccVertices.get();
			if (accVertices.contains(orig)) {
				accVertices.addAll(allReachableVertices);
			}
		}
	}

	/**
	 * Defines that dest is reachable from orig.
	 *
	 * @param orig
	 *            orig
	 * @param dest
	 *            dest
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
			if (!this.reachableMap.get(elem).isPresent()) {
				this.reachableMap.put(elem, new HashSet<T>());
			}
		}
	}

	public Set<T> getDirectSuccessors(T vertex) {
		Set<T> ret = new HashSet<>();
		OptMap<T, Set<T>> map = makeMapWithoutEquivalentVertices();
		Set<T> connectedVertices = new HashSet<>();
		Optional<Set<T>> optVertices = map.get(vertex);
		if (optVertices.isPresent()) {
			connectedVertices.addAll(optVertices.get());
		}
		OptMap<T, Integer> count = new OptMapImpl<>(new HashMap<>());
		for (T elem : connectedVertices) {
			Optional<Set<T>> optVerticesOfElem = map.get(elem);
			assert (optVerticesOfElem.isPresent());
			count.put(elem, optVerticesOfElem.get().size());
		}

		boolean changed = true;
		while (changed) {
			changed = false;
			T otherVertex = null;
			Iterator<T> it = connectedVertices.iterator();
			while ((otherVertex == null) && it.hasNext()) {
				T elem = it.next();
				Optional<Integer> optCountOfElem = count.get(elem);
				assert (optCountOfElem.isPresent());
				if (optCountOfElem.get() == 0) {
					otherVertex = elem;
				}
			}
			if (otherVertex != null) {
				changed = true;
				boolean directlyConnected = true;
				for (T elem : connectedVertices) {
					Optional<Set<T>> optVerticesOfElem = map.get(elem);
					assert (optVerticesOfElem.isPresent());
					if (optVerticesOfElem.get().contains(otherVertex)) {
						Optional<Integer> optCountOfElem = count.get(elem);
						assert (optCountOfElem.isPresent());
						count.put(elem, optCountOfElem.get() - 1);
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
		Set<Set<T>> ret = new HashSet<>();
		Set<T> visited = new HashSet<>();
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
	 *            orig
	 * @return the equivalent vertices of a particular vertex
	 */
	public Set<T> getEquivalentVertices(T orig) {
		Set<T> ret = new HashSet<>();
		ret.add(orig);
		Optional<Set<T>> optVertices = this.reachableMap.get(orig);
		if (optVertices.isPresent()) {
			for (T otherElem : optVertices.get()) {
				Optional<Set<T>> optOtherVertices = this.reachableMap.get(otherElem);
				assert (optOtherVertices.isPresent());
				if (optOtherVertices.get().contains(orig)) {
					ret.add(otherElem);
				}
			}
		}
		return ret;
	}

	public Set<T> getReachableVertices(T orig) {
		Set<T> ret = Collections.emptySet();
		Optional<Set<T>> optVertices = this.reachableMap.get(orig);
		if (optVertices.isPresent()) {
			ret = optVertices.get();
		}
		return Collections.unmodifiableSet(ret);
	}

	public Set<T> getVertices() {
		return Collections.unmodifiableSet(this.reachableMap.keySet());
	}

	protected OptMap<T, Set<T>> makeMapWithoutEquivalentVertices() {
		OptMap<T, Set<T>> ret = new OptMapImpl<>(new HashMap<>());
		for (T elem : getVertices()) {
			Set<T> otherSet = new HashSet<>();
			Optional<Set<T>> optVertices = this.reachableMap.get(elem);
			assert optVertices.isPresent();
			otherSet.addAll(optVertices.get());
			if (otherSet.contains(elem)) {
				otherSet.removeAll(getEquivalentVertices(elem));
			}
			ret.put(elem, otherSet);
		}
		return ret;
	}

	@Override
	public String toString() {
		StringBuffer ret = new StringBuffer();
		for (T vertex : getVertices()) {
			ret.append("(" + vertex.toString() + ") > (");
			Optional<Set<T>> optVertices = this.reachableMap.get(vertex);
			assert (optVertices.isPresent());
			for (T otherVertex : optVertices.get()) {
				ret.append(" (" + otherVertex.toString() + ") ");
			}
			ret.append(")\n");
		}
		return ret.toString();
	}

}
