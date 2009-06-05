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
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * This thread starts a new instance of the CEL server. All the platform
 * dependent constraints should be only in this class. This thread ends as soon
 * as CEL has been started. The temporary files are marked with deleteOnExit().
 * 
 * @author Julian Mendez
 */
class CelProcessThread extends Thread {

	private static final Logger logger = Logger.getAnonymousLogger();

	/** Location inside the bundle. */
	private static String bundleLocation = "native";

	/** Commands to maximize the priority. */
	private static final String nicePrefix = "nice -n 0";

	/** Command to start CEL. */
	private static String celCommandLine = "cel -owlapiServer localhost";

	/** Suffix to indicate logging mode. */
	private static String loggingSuffix = "logging";

	/** Main file of CEL binary. */
	private static final String celMain = "cel";

	/** Compiled image of CEL binary. */
	private static final String celImage = "cel.dxl";

	/** License for CEL binary. */
	private static final String lispLicense = "cel.lic";

	/** Linux library for Allegro Common Lisp. */
	private static final String lispLibrary = "libacli817.so";

	/** Port used to communicate with Java. */
	private int port = 0;

	/**
	 * Small gap in milliseconds between different commands of the operating
	 * system.
	 */
	private int timeGap = 100;

	private Process process = null;
	private CelSocket celSocket = null;

	public CelProcessThread(int port) {
		this.port = port;
	}

	public int getPort() {
		return this.port;
	}

	public Process getProcess() {
		return this.process;
	}

	public CelSocket getCelSocket() {
		return this.celSocket;
	}

	/**
	 * Tells when the operating system *seems* to be Unix-like.
	 * 
	 * @return true if the operating system seems to be a Unix
	 */
	protected boolean isUnixlikePlatform() {
		boolean ret = System.getProperty("file.separator").equals("/")
				&& System.getProperty("path.separator").equals(":")
				&& System.getProperty("line.separator").equals("\n");
		return ret;
	}

	/**
	 * Decompresses one file into a directory, marking it with deleteOnExit().
	 */
	protected void decompressFile(String location, String filename,
			File directory) throws IOException {
		BufferedInputStream source = new BufferedInputStream(
				CelProcessThread.class.getClassLoader().getResourceAsStream(
						location + "/" + filename));
		File newFile = new File(directory, filename);
		newFile.deleteOnExit();
		BufferedOutputStream out = new BufferedOutputStream(
				new FileOutputStream(newFile));
		for (int ch = 0; ch != -1;) {
			ch = source.read();
			if (ch != -1) {
				out.write(ch);
			}
		}
		out.flush();
		out.close();
		source.close();
	}

	/**
	 * Installs CEL files in a temporary directory. These files will be deleted
	 * using deleteOnExit().
	 */
	protected File createBinary() throws IOException, InterruptedException {
		File tempDirectory = File.createTempFile(celMain, "");
		tempDirectory.delete();
		tempDirectory.mkdir();
		tempDirectory.deleteOnExit();
		decompressFile(bundleLocation, celMain, tempDirectory);
		decompressFile(bundleLocation, celImage, tempDirectory);
		decompressFile(bundleLocation, lispLicense, tempDirectory);
		decompressFile(bundleLocation, lispLibrary, tempDirectory);
		return tempDirectory;
	}

	/**
	 * Starts a new instance of the CEL server.
	 */
	public void run() {
		try {
			File celDirectory = createBinary();
			logger.fine("Creating CEL directory "
					+ celDirectory.getAbsolutePath());
			if (!isUnixlikePlatform()) {
				throw new RuntimeException("CEL cannot run on this platform.");
			}
			String command = "chmod +x " + celDirectory.getAbsolutePath()
					+ "/cel";
			logger.fine("Unix-like platform found, running '" + command + "'.");
			Thread.sleep(timeGap);
			Runtime.getRuntime().exec(command);
			Thread.sleep(timeGap);
			String message = nicePrefix + " " + celDirectory.getAbsolutePath()
					+ System.getProperty("file.separator") + celCommandLine
					+ " " + port;
			if (logger.getParent() != null
					&& logger.getParent().getLevel() != null
					&& logger.getParent().getLevel().intValue() < Level.INFO
							.intValue()) {
				message = message + " " + loggingSuffix;
			}
			logger.fine("Trying '" + message + "'.");
			this.process = Runtime.getRuntime().exec(message);
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
