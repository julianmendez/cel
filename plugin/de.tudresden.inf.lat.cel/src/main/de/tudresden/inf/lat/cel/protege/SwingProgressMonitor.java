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

package de.tudresden.inf.lat.cel.protege;

import org.semanticweb.owl.util.ProgressMonitor;

/**
 * This class is a progress monitor that shows the progress using Swing
 * components.
 */
class SwingProgressMonitor implements ProgressMonitor {

	private static final int DEFAULT_MAXIMUM = 100;
	private static final int MINIMUM = 0;

	private String message = "";
	private javax.swing.ProgressMonitor monitor = new javax.swing.ProgressMonitor(
			null, "", "", MINIMUM, DEFAULT_MAXIMUM);

	private long progress = 0;

	public SwingProgressMonitor() {
		setProgress(0);
	}

	public boolean isCancelled() {
		return this.monitor.isCanceled();
	}

	public void setFinished() {
		setProgress(this.monitor.getMaximum());
		this.monitor.close();
	}

	public void setIndeterminate(boolean ind) {
	}

	public void setMessage(String msg) {
		this.message = msg;
	}

	public void setProgress(long prog) {
		this.progress = prog;
		this.monitor.setProgress((int) prog);
		updateTitle();
	}

	public void setSize(long s) {
		this.monitor.setMaximum((int) s);
	}

	public void setStarted() {
		setProgress(0);
	}

	protected void updateTitle() {
		this.monitor.setNote(this.message + " "
				+ (100 * progress / (this.monitor.getMaximum() - MINIMUM))
				+ "%");
	}
}
