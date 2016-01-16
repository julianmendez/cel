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

package de.tudresden.inf.lat.cel.connection;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.logging.Logger;

import de.tudresden.inf.lat.jsexp.Sexp;
import de.tudresden.inf.lat.jsexp.SexpFactory;
import de.tudresden.inf.lat.jsexp.SexpParserException;

/**
 * This class encapsulates the communication with CEL reasoner in Lisp. Its main
 * purpose is to send and receive S-expressions.
 * 
 * @author Julian Mendez
 * 
 */
class CelSocket {

	private static final Logger logger = Logger.getLogger(CelSocket.class.getName());

	public static final String protError = ":ERROR";
	public static final String protOther = ":OWLAPI-RETURN";
	public static final String protReturn = ":RETURN";
	public static final String protVoid = ":VOID";

	private Socket socket = null;

	/**
	 * Creates a new CelSocket.
	 * 
	 * @param socket
	 *            this socket must be already connected to the CEL reasoner in
	 *            Lisp.
	 */
	public CelSocket(Socket socket) {
		if (socket == null) {
			throw new IllegalArgumentException("Socket cannot be null.");
		}
		this.socket = socket;
	}

	/**
	 * Closes the connection to the CEL reasoner in Lisp.
	 * 
	 * @throws IOException
	 */
	public void close() throws IOException {
		getSocket().close();
	}

	protected Socket getSocket() {
		return this.socket;
	}

	protected Sexp process(Sexp original) throws CelConnectionException {
		Sexp ret = null;
		if (original.getLength() > 0) {
			String msgid = original.get(0).toString();
			if (msgid.equalsIgnoreCase(protReturn)) {
				ret = original.get(1);
			} else if (msgid.equalsIgnoreCase(protError)) {
				logger.fine("error " + original.get(1) + "'.");
				throw new CelConnectionException("Error received: '" + original.get(1) + "'.");
			} else if (msgid.equalsIgnoreCase(protVoid)) {
				ret = SexpFactory.newNonAtomicSexp();
			} else if (msgid.equalsIgnoreCase(protOther)) {
				ret = SexpFactory.newNonAtomicSexp();
			} else {
				throw new CelConnectionException("Message identification not recognized: '" + msgid + "'.");
			}
		} else {
			throw new CelConnectionException("Unexpected response format: '" + original + "'.");
		}
		return ret;
	}

	/**
	 * Sends an S-expression to the CEL reasoner in Lisp, and returns and
	 * S-expression as response.
	 * 
	 * @param expr
	 *            S-expression to be sent to the CEL reasoner in Lisp.
	 * @return resulting response from the CEL reasoner in Lisp.
	 * @throws CelConnectionException
	 *             if there is any exception in the process.
	 */
	public Sexp send(Sexp expr) throws CelConnectionException {
		Sexp ret = null;
		try {
			String request = expr.toString();
			getSocket().getOutputStream().write(request.getBytes());
			getSocket().getOutputStream().flush();
			Sexp response = SexpFactory.parse(new BufferedInputStream(getSocket().getInputStream()));
			String str = "sent '" + expr + "'  received '" + response + "'     ";
			logger.finer(str);
			ret = process(response);
		} catch (IOException e) {
			throw new CelConnectionException(e);
		} catch (SexpParserException e) {
			throw new CelConnectionException(e);
		}
		return ret;
	}
}
