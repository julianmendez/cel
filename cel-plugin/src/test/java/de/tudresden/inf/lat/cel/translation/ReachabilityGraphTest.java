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
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import de.tudresden.inf.lat.util.map.OptMap;
import de.tudresden.inf.lat.util.map.OptMapImpl;

/**
 * This class tests a saturated directed graph.
 * 
 * @author Julian Mendez
 */
public class ReachabilityGraphTest {

	protected OptMap<String, Set<String>> createDirectlyConnected() {
		OptMap<String, Set<String>> ret = new OptMapImpl<>(new HashMap<>());
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
		Set<String> set1 = new HashSet<>();
		set1.add("a11");
		set1.add("a12");
		ret.put("a06", set1);
		Set<String> set2 = new HashSet<>();
		set2.add("a05");
		set2.add("a09");
		set2.add("a13");
		set2.add("a14");
		ret.put("a08", set2);
		return ret;
	}

	protected Set<Set<String>> createEquivalentClasses() {
		Set<Set<String>> ret = new HashSet<>();
		ret.add(Collections.singleton("a01"));
		ret.add(Collections.singleton("a02"));
		ret.add(Collections.singleton("a03"));
		ret.add(Collections.singleton("a04"));
		ret.add(Collections.singleton("a06"));
		ret.add(Collections.singleton("a07"));
		ret.add(Collections.singleton("a08"));
		ret.add(Collections.singleton("a10"));
		ret.add(Collections.singleton("a13"));
		Set<String> set1 = new HashSet<>();
		set1.add("a05");
		set1.add("a09");
		set1.add("a14");
		ret.add(set1);
		Set<String> set2 = new HashSet<>();
		set2.add("a11");
		set2.add("a12");
		ret.add(set2);
		return ret;
	}

	protected ReachabilityGraph<String> createExtendedInstance() {
		ReachabilityGraph<String> ret = createInstance();
		ret.addReachable("a10", "a1");
		return ret;
	}

	protected ReachabilityGraph<String> createInstance() {
		ReachabilityGraph<String> ret = new ReachabilityGraph<>();
		ret.addReachable("a01", "a03");
		ret.addReachable("a02", "a03");
		ret.addReachable("a03", "a08");
		ret.addReachable("a04", "a06");
		ret.addReachable("a05", "a09");
		ret.addReachable("a05", "a10");
		ret.addReachable("a06", "a12");
		ret.addReachable("a07", "a08");
		ret.addReachable("a08", "a04");
		ret.addReachable("a08", "a05");
		ret.addReachable("a08", "a13");
		ret.addReachable("a09", "a14");
		ret.addReachable("a12", "a11");
		ret.addReachable("a13", "a04");
		ret.addReachable("a11", "a12");
		ret.addReachable("a14", "a05");
		return ret;
	}

	protected Set<String> createNodeSet() {
		Set<String> ret = new HashSet<>();
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

	@Test
	public void testDirectSuccessor() {
		ReachabilityGraph<String> graph = createInstance();
		OptMap<String, Set<String>> map = createDirectlyConnected();
		graph.getVertices().forEach(elem -> {
			Assert.assertTrue(map.get(elem).isPresent());
			Assert.assertEquals(map.get(elem).get(), graph.getDirectSuccessors(elem));
		});
		Assert.assertTrue(true);
	}

	@Test
	public void testEquivalentClasses() {
		ReachabilityGraph<String> graph = createInstance();
		Assert.assertEquals(createEquivalentClasses(), graph.getEquivalentClasses());
	}

	@Test
	public void testEquivalentVertices() {
		ReachabilityGraph<String> graph = createInstance();
		Set<String> set1 = new HashSet<>();
		set1.add("a05");
		set1.add("a09");
		set1.add("a14");
		Assert.assertEquals(set1, graph.getEquivalentVertices("a05"));
		Assert.assertEquals(set1, graph.getEquivalentVertices("a09"));
		Assert.assertEquals(set1, graph.getEquivalentVertices("a14"));
		Set<String> set2 = new HashSet<>();
		set2.add("a11");
		set2.add("a12");
		Assert.assertEquals(set2, graph.getEquivalentVertices("a11"));
		Assert.assertEquals(set2, graph.getEquivalentVertices("a12"));
		graph.getVertices() //
				.stream() //
				.filter(node -> (!set1.contains(node) && !set2.contains(node))) //
				.forEach(node -> {
					Assert.assertEquals(Collections.singleton(node), graph.getEquivalentVertices(node));
				});
	}

	@Test
	public void testGetVertices() {
		ReachabilityGraph<String> graph = createInstance();
		Assert.assertEquals(createNodeSet(), graph.getVertices());
	}

}
