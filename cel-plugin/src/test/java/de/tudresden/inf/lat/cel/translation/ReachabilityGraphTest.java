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
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * This class tests a saturated directed graph.
 * 
 * @author Julian Mendez
 */
public class ReachabilityGraphTest extends TestCase {

	protected Map<String, Set<String>> createDirectlyConnected() {
		Map<String, Set<String>> ret = new HashMap<String, Set<String>>();
		ret.put("a10", new HashSet<String>());
		ret.put("a11", new HashSet<String>());
		ret.put("a12", new HashSet<String>());
		ret.put("a01", Collections.singleton("a03"));
		ret.put("a02", Collections.singleton("a03"));
		ret.put("a03", Collections.singleton("a08"));
		ret.put("a04", Collections.singleton("a06"));
		ret.put("a05", Collections.singleton("a10"));
		ret.put("a07", Collections.singleton("a08"));
		ret.put("a09", Collections.singleton("a10"));
		ret.put("a13", Collections.singleton("a04"));
		ret.put("a14", Collections.singleton("a10"));
		Set<String> set1 = new HashSet<String>();
		set1.add("a11");
		set1.add("a12");
		ret.put("a06", set1);
		Set<String> set2 = new HashSet<String>();
		set2.add("a05");
		set2.add("a09");
		set2.add("a13");
		set2.add("a14");
		ret.put("a08", set2);
		return ret;
	}

	protected Set<Set<String>> createEquivalentClasses() {
		Set<Set<String>> ret = new HashSet<Set<String>>();
		ret.add(Collections.singleton("a01"));
		ret.add(Collections.singleton("a02"));
		ret.add(Collections.singleton("a03"));
		ret.add(Collections.singleton("a04"));
		ret.add(Collections.singleton("a06"));
		ret.add(Collections.singleton("a07"));
		ret.add(Collections.singleton("a08"));
		ret.add(Collections.singleton("a10"));
		ret.add(Collections.singleton("a13"));
		Set<String> set1 = new HashSet<String>();
		set1.add("a05");
		set1.add("a09");
		set1.add("a14");
		ret.add(set1);
		Set<String> set2 = new HashSet<String>();
		set2.add("a11");
		set2.add("a12");
		ret.add(set2);
		return ret;
	}

	protected ReachabilityGraph<String> createExtendedInstance() {
		ReachabilityGraph<String> ret = createInstance();
		ret.addReachable ("a10", "a1");
		return ret;
	}

	protected ReachabilityGraph<String> createInstance() {
		ReachabilityGraph<String> ret = new ReachabilityGraph<String>();
		ret.addReachable ("a01", "a03");
		ret.addReachable ("a02", "a03");
		ret.addReachable ("a03", "a08");
		ret.addReachable ("a04", "a06");
		ret.addReachable ("a05", "a09");
		ret.addReachable ("a05", "a10");
		ret.addReachable ("a06", "a12");
		ret.addReachable ("a07", "a08");
		ret.addReachable ("a08", "a04");
		ret.addReachable ("a08", "a05");
		ret.addReachable ("a08", "a13");
		ret.addReachable ("a09", "a14");
		ret.addReachable ("a12", "a11");
		ret.addReachable ("a13", "a04");
		ret.addReachable ("a11", "a12");
		ret.addReachable ("a14", "a05");
		return ret;
	}

	protected Set<String> createNodeSet() {
		Set<String> ret = new HashSet<String>();
		ret.add("a01");
		ret.add("a02");
		ret.add("a03");
		ret.add("a04");
		ret.add("a05");
		ret.add("a06");
		ret.add("a07");
		ret.add("a08");
		ret.add("a09");
		ret.add("a10");
		ret.add("a11");
		ret.add("a12");
		ret.add("a13");
		ret.add("a14");
		return ret;
	}

	public void testDirectSuccessor() {
		ReachabilityGraph<String> graph = createInstance();
		Map<String, Set<String>> map = createDirectlyConnected();
		for (String elem : graph.getVertices()) {
			assertEquals(map.get(elem), graph.getDirectSuccessors(elem));
		}
		assertTrue(true);
	}

	public void testEquivalentClasses() {
		ReachabilityGraph<String> graph = createInstance();
		assertEquals(createEquivalentClasses(), graph.getEquivalentClasses());
	}

	public void testEquivalentVertices() {
		ReachabilityGraph<String> graph = createInstance();
		Set<String> set1 = new HashSet<String>();
		set1.add("a05");
		set1.add("a09");
		set1.add("a14");
		assertEquals(set1, graph.getEquivalentVertices("a05"));
		assertEquals(set1, graph.getEquivalentVertices("a09"));
		assertEquals(set1, graph.getEquivalentVertices("a14"));
		Set<String> set2 = new HashSet<String>();
		set2.add("a11");
		set2.add("a12");
		assertEquals(set2, graph.getEquivalentVertices("a11"));
		assertEquals(set2, graph.getEquivalentVertices("a12"));
		for (String node : graph.getVertices()) {
			if (!set1.contains(node) && !set2.contains(node)) {
				assertEquals(Collections.singleton(node), graph
						.getEquivalentVertices(node));
			}
		}
	}

	public void testGetVertices() {
		ReachabilityGraph<String> graph = createInstance();
		assertEquals(createNodeSet(), graph.getVertices());
	}
}
