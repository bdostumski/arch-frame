#!/usr/bin/env sh

config_apparmor() {
    # Enable AppArmor at boot (systemd)
    sudo systemctl enable --now apparmor.service

    # Enable auditd for better logging (optional; comment out if you don't want it)
    sudo systemctl enable --now auditd.service || true

    # Load all profiles that come with the package
    # (Some systems use `apparmor_parser`/`aa-enforce`; this is a safe baseline.)
    if command -v aa-enforce >/dev/null 2>&1; then
        # Enforce all profiles under /etc/apparmor.d that are loadable
        sudo aa-enforce /etc/apparmor.d/* 2>/dev/null || true
    fi
}
