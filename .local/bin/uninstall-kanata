#!/bin/bash

# ==============================================================================
# Script to REMOVE the hardened Kanata setup created by setup-kanata-hardened.sh
# WARNING: This is irreversible. Use with caution.
# ==============================================================================

# Configuration (should match names used in setup script)
KANATA_USER="kanata"
SERVICE_FILE="/etc/systemd/system/kanata.service"
UDEV_RULE="/etc/udev/rules.d/50-kanata.rules"
KANATA_BIN="/usr/local/bin/kanata"
CONFIG_DIR="/etc/kanata"

# FUNCTION TO PRINT MESSAGES
echoinfo() {
    echo "[INFO] $1"
}
echoerror() {
    echo "[ERROR] $1" >&2
}

# CHECK FOR ROOT/SUDO
if [ "$(id -u)" -ne 0 ]; then
  echoerror "This script must be run with sudo or as root."
  exit 1
fi

echoinfo "Starting removal of hardened Kanata setup..."
echoinfo "WARNING: This process is irreversible."

# STEP 1: Stop and Disable Service
echoinfo "Stopping and disabling kanata.service..."
systemctl stop kanata.service > /dev/null 2>&1
systemctl disable kanata.service > /dev/null 2>&1
echoinfo "Service stopped and disabled (errors ignored)."

# STEP 2: Remove Service File
echoinfo "Removing systemd service file: $SERVICE_FILE..."
rm -f "$SERVICE_FILE"

# STEP 3: Reload Systemd
echoinfo "Reloading systemd daemon..."
systemctl daemon-reload
systemctl reset-failed > /dev/null 2>&1 # Clean up failed state if any

# STEP 4: Remove Kanata Executable
echoinfo "Removing Kanata executable: $KANATA_BIN..."
rm -f "$KANATA_BIN"

# STEP 5: Remove udev Rule
echoinfo "Removing udev rule: $UDEV_RULE..."
rm -f "$UDEV_RULE"
# Optionally reload udev rules (often not strictly necessary for removal)
# echoinfo "Reloading udev rules..."
# udevadm control --reload-rules > /dev/null 2>&1

# STEP 6: Remove Configuration Directory
echoinfo "Removing configuration directory: $CONFIG_DIR..."
rm -rf "$CONFIG_DIR"

# STEP 7: Delete User
# This should also remove the user from secondary groups like 'input' and 'uinput'.
# If the primary group 'kanata' was created via '--user-group' and is now empty,
# 'userdel' often removes it too.
if id "$KANATA_USER" > /dev/null 2>&1; then
    echoinfo "Deleting user '$KANATA_USER'..."
    userdel "$KANATA_USER"
    if id "$KANATA_USER" > /dev/null 2>&1; then
        echoerror "Failed to delete user '$KANATA_USER'. Manual removal might be needed (userdel $KANATA_USER)."
    else
        echoinfo "User '$KANATA_USER' deleted."
    fi
else
    echoinfo "User '$KANATA_USER' does not exist, skipping deletion."
fi

# Note: We are intentionally NOT deleting the 'uinput' group as it might be used by others.

echoinfo "-----------------------------------------------------"
echoinfo "Kanata hardened setup removal script finished."
echoinfo "A REBOOT is recommended to ensure all changes are fully applied."
echoinfo "-----------------------------------------------------"

exit 0
