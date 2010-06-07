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

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;

import de.tudresden.inf.lat.jsexp.Sexp;

/**
 * This class initializes the Java server for CEL. In the process, it creates
 * the corresponding socket for communicating with the reasoner in Lisp.
 * 
 * @author Julian Mendez
 * 
 */
public class CelSocketManager implements CelOutputListener {

	/** Greeting message presented when the program starts. */
	public static final String greetingMessage = ""
			+ "\nCEL: [C]lassifier for the Description Logic [E][L]+ "
			+ "\nCopyright (C) 2005-2009: B. Suntisrivaraporn and TU Dresden."
			+ "\nOWL API - Copyright (C) 2009: Julian Mendez."
			+ "\nCEL comes with ABSOLUTELY NO WARRANTY; use at your own risk."
			+ "\nThis is free software for research and evaluation purposes."
			+ "\nCommercial use is prohibited; please contact the author."
			+ "\n";

	private static final Logger logger = Logger
			.getLogger(CelSocketManager.class.getName());

	private CelSocket celSocket = null;

	/** Time given to CEL to finish the connection. */
	private int closingConnectionTime = 1000;

	/** First port to try to open a socket. */
	private int firstPort = 40000;

	/** Last port to try to open a socket. */
	private int lastPort = 50000;

	private CelProcessOutputHandler outputHandler = null;
	private CelProcessThread processThread = null;
	private ReasonerProgressMonitor progressMonitor = null;
	/**
	 * Timeout in milliseconds used to determine whether there is client (the
	 * CEL reasoner in Lisp) trying to connect to the server (a Java thread).
	 */
	private int timeoutForServer = 10000;

	public CelSocketManager() {
	}

	private CelSocket getCelSocket() {
		return this.celSocket;
	}

	public CelProcessOutputHandler getOutputHandler() {
		return this.outputHandler;
	}

	protected Process getProcess() {
		return getProcessThread().getProcess();
	}

	protected CelProcessThread getProcessThread() {
		return this.processThread;
	}

	public ReasonerProgressMonitor getProgressMonitor() {
		return this.progressMonitor;
	}

	@Override
	public void notifyCancelButtonPressed() {
		getOutputHandler().stopExecution();
	}

	@Override
	public void notifyExecutionFinished() {
		if (getProcessThread() != null) {
			getProcessThread().stopProcess();
		}
	}

	/**
	 * Sends an S-expression to the CEL reasoner, starting this manager if
	 * necessary.
	 * 
	 * @param message
	 *            message to be sent.
	 * @return the answer given by the CEL reasoner.
	 * @throws CelConnectionException
	 */
	public Sexp send(Sexp message) throws CelConnectionException {
		Sexp response = null;
		if (getCelSocket() == null) {
			start();
		}
		response = getCelSocket().send(message);
		return response;
	}

	public void setProgressMonitor(ReasonerProgressMonitor monitor) {
		this.progressMonitor = monitor;
		if (getOutputHandler() != null) {
			getOutputHandler().setProgressMonitor(progressMonitor);
		}
	}

	/**
	 * Starts the execution.
	 */
	public void start() {
		int port = -1;
		ServerSocket server = null;
		boolean found = false;
		for (int index = this.firstPort; !found && index < this.lastPort; index++) {
			try {
				server = new ServerSocket(index);
				port = index;
				found = true;
			} catch (IOException e) {
			}
		}
		if (!found) {
			throw new RuntimeException("No available ports between "
					+ firstPort + " and " + lastPort + ".");
		}
		try {
			server.setSoTimeout(timeoutForServer);
			logger.fine("Java CEL server waits for Lisp CEL clients on port "
					+ port + " for " + timeoutForServer + " milliseconds.");
			this.processThread = new CelProcessThread(port);
			this.processThread.start();
			Socket connection = server.accept();
			this.celSocket = new CelSocket(connection);
			logger.fine("Java CEL server received a client on port " + port
					+ ".");
			if (logger.isLoggable(Level.CONFIG)) {
				System.out.println(greetingMessage);
			}
			this.outputHandler = new CelProcessOutputHandler(getProcess()
					.getInputStream(), System.out);
			getOutputHandler().setProgressMonitor(getProgressMonitor());
			getOutputHandler().setOutputListener(this);
			getOutputHandler().start();
		} catch (IOException e) {
			logger.log(Level.SEVERE,
					"Java CEL server cannot open a client on port " + port
							+ ".", e);
			throw new RuntimeException(e);
		}
	}

	/**
	 * Stops the execution.
	 * 
	 * @param endMessage
	 *            command required by the CEL reasoner to finish its execution.
	 */
	public void stopExecution(Sexp endMessage) {
		if (getCelSocket() != null && !getCelSocket().getSocket().isClosed()) {
			try {
				try {
					send(endMessage);
				} catch (CelConnectionException e) {
					logger.log(Level.WARNING,
							"CEL server rejected dispose command.", e);
				}
				Thread.sleep(closingConnectionTime);
				getCelSocket().getSocket().close();
			} catch (IOException e) {
				logger.log(Level.SEVERE, "IO exception", e);
			} catch (InterruptedException e) {
				logger.log(Level.SEVERE, "interruption", e);
			}
		} else {
			logger.fine("Connection already closed.");
		}
		if (getOutputHandler() != null) {
			getOutputHandler().stopExecution();
		}
	}
}
