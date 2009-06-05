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

import de.tudresden.inf.lat.cel.connection.CelProgressMonitor;

/**
 * This class is a progress monitor that shows the progress using Swing
 * components.
 */
class SwingProgressMonitor implements CelProgressMonitor {

	private static final int DEFAULT_MAXIMUM = 100;
	private static final int MINIMUM = 0;

	private boolean active = false;
	private String message = "";
	private long progress = 0;
	private javax.swing.ProgressMonitor monitor = null;

	public SwingProgressMonitor() {
		init();
	}

	public String getMessage() {
		return this.message;
	}

	public void setMessage(String msg) {
		this.message = msg;
	}

	public long getProgress() {
		return this.progress;
	}

	public void setProgress(long prog) {
		if (active) {
			this.progress = prog;
			this.monitor.setProgress((int) prog);
			updateTitle();
		}
	}

	public long getSize() {
		return this.monitor.getMaximum();
	}

	public void setSize(long s) {
		this.monitor.setMaximum((int) s);
	}

	/**
	 * Increases by 1 the level of progress.
	 */
	public void increment() {
		setProgress(getProgress() + 1);
	}

	protected void init() {
		this.monitor = new javax.swing.ProgressMonitor(null, "", "", MINIMUM,
				DEFAULT_MAXIMUM);
		this.progress = 0;
		setProgress(this.progress);
	}

	public boolean isCancelled() {
		return this.monitor.isCanceled();
	}

	public void setStarted() {
		active = true;
		init();
	}

	public void setFinished() {
		active = false;
		this.monitor.close();
	}

	public void setIndeterminate(boolean ind) {
	}

	protected void updateTitle() {
		this.monitor.setNote(getMessage() + " "
				+ (100 * progress / (getSize() - MINIMUM)) + "%");
	}
}
