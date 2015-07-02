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

package de.tudresden.inf.lat.cel.owlapi;

/**
 * This exception should be thrown when it is invoked an operation that is not
 * supported by CEL.
 * 
 * @author Julian Mendez
 */
public class UnsupportedReasonerOperationInCelException extends
		RuntimeException {

	private static final long serialVersionUID = 731968259094527192L;

	public UnsupportedReasonerOperationInCelException() {
		super("This operation is not supported by CEL.");
	}

	public UnsupportedReasonerOperationInCelException(String message) {
		super(message);
	}

}
