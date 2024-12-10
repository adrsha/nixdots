const mpris = await Service.import("mpris");
const bluetooth = await Service.import("bluetooth");
const network = await Service.import("network");
const hyprland = await Service.import("hyprland");
const audio = await Service.import("audio");
const { query } = await Service.import("applications");
const battery = await Service.import("battery");
const systemtray = await Service.import("systemtray");
const notifications = await Service.import("notifications");

const hyprlandClientWidget = () => {
    // Function to get current clients and active window
    const getCurrentClients = () => {
        try {
            // Get all clients from Hyprctl
            const clientsJson = Utils.exec('hyprctl clients -j');
            const clients = JSON.parse(clientsJson);
            
            // Get active window
            const activeWindowJson = Utils.exec('hyprctl activewindow -j');
            const activeWindow = activeWindowJson ? JSON.parse(activeWindowJson) : null;

            return { 
                clients, 
                activeWindow: activeWindow
            };
        } catch (err) {
            print(`Error fetching clients: ${err}`);
            return { clients: [], activeWindow: null };
        }
    };

    // Create a reactive variable for clients
    const clientsState = Variable(getCurrentClients());

    // Create an event listener
    const eventListener = hyprland.connect('event', (_, event) => {
        // List of events that should trigger an update
        const updateEvents = [
            'openwindow', 
            'closewindow', 
            'activewindow', 
            'changeworkspace'
        ];

        // Check if the event requires an update
        if (updateEvents.some(evt => event.includes(evt))) {
            clientsState.setValue(getCurrentClients());
        }
    });

    // Icon mapping function
    const getClientIcon = (clientClass) => {
        const lowercaseClass = clientClass.toLowerCase();
        const iconMappings = {
            'zen-alpha': 'zen',
            'godot': 'godot',
            'chromium': 'google-chrome',
            'code': 'visual-studio-code',
            'alacritty': 'alacritty',
            'spotify': 'spotify',
            'default': 'application-x-executable'
        };
        
        return iconMappings[lowercaseClass] || iconMappings['default'];
    };

    // Create the box widget
    return Widget.Box({
        className: 'hyprland-clients-container',
        vertical: false,
        children: clientsState.bind().as(state => 
            state.clients.map(client => {
                const isActive = state.activeWindow && state.activeWindow.address === client.address;
                return Widget.Button({
                    className: `client-button ${
                        isActive ? 'active-client' : 'inactive-client'
                    }`,
                    tooltipText: `${client.title || 'Untitled'} (${client.class})`,
                    child: Widget.Box({
                        vertical: false,
                        children: [
                            Widget.Icon({
                                icon: getClientIcon(client.class),
                                size: 32,
                                className: 'client-icon'
                            }),
                            // Conditionally add title for active window
                            ...(isActive && client.title ? [
                                Widget.Label({
                                    label: client.title,
                                    className: 'active-window-title',
                                    maxWidthChars: 20,
                                    truncate: 'end'
                                })
                            ] : [])
                        ]
                    }),
                    onClicked: () => {
                        try {
                            hyprland.messageAsync(`dispatch focuswindow address:${client.address}`);
                        } catch (err) {
                            print(`Failed to focus client: ${err}`);
                        }
                    }
                });
            })
        )
    });
};

// Create the widget
const hyprlandClient = hyprlandClientWidget();

// Optional: Periodically update clients
// Utils.interval(1000, () => {
//     // You might want to adjust the interval or use Hyprland's event system
//     clientsState.setValue(getCurrentClients());
// });


const Music = () => {
    const playbackStatus = Variable('Stopped', {
        poll: [1000, () => {
            try {
                return Utils.exec('playerctl status');
            } catch {
                return 'Stopped';
            }
        }]
    });
    const music = Variable('', {
        poll: [1000, () => {
            const playerctlOutput = Utils.exec('playerctl metadata');
            const titleMatch = playerctlOutput.match(/xesam:title\s+(.+)/);
            const artistMatch = playerctlOutput.match(/xesam:artist\s+(.+)/);
            try {
                if (titleMatch && artistMatch) {
                    return `${artistMatch[1]} - ${titleMatch[1]}`;
                } else {
                    return '∅';
                }
            } catch {
                return '∅';
            }
        }]
    });
        return Widget.Box({
            className: 'musicWidgetContainer',
            children: [
                Widget.Icon({
                    className: 'musicWidgetIcon',
                    icon: playbackStatus.bind().transform(status => 
                        status === 'Playing' 
                            ? 'media-playback-pause-symbolic' 
                            : 'media-playback-start-symbolic'
                    ),
                }),
                Widget.Label({
                    class_name: 'musicWidget',
                    max_width_chars: 30,
                    truncate: 'end',
                    label: music.bind()
                })
            ],
        });
};

function Notification(n) {
    let notif_icon;
    if (n.image) {
        notif_image = Widget.Box({
            css:
            `background-image: url("${n.image}");` +
                "background-size: contain;" +
                "background-repeat: no-repeat;" +
                "background-position: center;",
        });
    } else if (Utils.lookUpIcon(n.app_icon)) {
        notif_icon = n.app_icon;
    } else if (n.app_entry && Utils.lookUpIcon(n.app_entry)) {
        notif_icon = n.app_entry;
    } else {
        notif_icon = "dialog-information";
    }
    const icon = Widget.Box({
        vpack: "start",
        class_name: "notificon",
        child: Widget.Icon(notif_icon),
    });
    notif_icon = icon;
    const title = Widget.Label({
        class_name: "notiftitle",
        xalign: 0,
        justification: "left",
        hexpand: true,
        max_width_chars: 24,
        truncate: "end",
        wrap: true,
        label: n.summary,
        use_markup: true,
    });
    const body = Widget.Label({
        class_name: "notifbody",
        hexpand: true,
        use_markup: true,
        xalign: 0,
        justification: "left",
        label: n.body,
        wrap: true,
    });

    const actions = Widget.Box({
        class_name: "notifactions",
        children: n.actions.map(({ id, label }) =>
            Widget.Button({
                class_name: "notifaction-button",
                on_clicked: () => {
                    n.invoke(id);
                    n.dismiss();
                },
                hexpand: true,
                child: Widget.Label(label),
            }),
        ),
    });

    return Widget.EventBox(
        {
            attribute: { id: n.id },
            on_primary_click: n.dismiss,
        },
        Widget.Box(
            {
                class_name: `notification ${n.urgency}`,
                vertical: true,
            },
            Widget.Box([icon, Widget.Box({ vertical: true }, title, body)]),
            actions,
        ),
    );
}

function NotificationsPopups(monitor = 0) {
    const list = Widget.Box({
        vertical: true,
        children: notifications.popups.map(Notification),
    });
    function onNotified(_, id) {
        const n = notifications.getNotification(id);
        if (n) list.children = [Notification(n), ...list.children];
    }

    function onDismissed(_, id) {
        list.children.find((n) => n.attribute.id === id)?.destroy();
    }

    list
        .hook(notifications, onNotified, "notified")
        .hook(notifications, onDismissed, "dismissed");

    return Widget.Window({
        monitor,
        layer: "overlay",
        name: `notifications${monitor}`,
        class_name: "notification-popups",
        anchor: ["bottom", "right"],
        child: Widget.Box({
            css: "min-width: 1px; min-height: 1px;",
            class_name: "notifications",
            vertical: true,
            child: list,
        }),
    });
}

function mainBar(monitor = 0) {
    const bluetoothWidget = () =>
        Widget.EventBox({
            className: "bluetoothWidgetContainerContainer",
            onPrimaryClick: () => {
                bluetooth.enabled = !bluetooth.enabled;
            },
            child: Widget.Box({
                className: "bluetoothWidgetContainer",
                children: [
                    Widget.Icon({
                        className: "bluetoothWidgetIcon",
                        icon: bluetooth
                        .bind("enabled")
                        .as((on) => `bluetooth-${on ? "active" : "disabled"}-symbolic`),
                    }),
                    Widget.Box({
                        className: "bluetoothWidget",
                        setup: (self) =>
                            self.hook(
                                bluetooth,
                                (self) => {
                                    self.children = bluetooth.connected_devices.map(
                                        ({ icon_name, name }) =>
                                            Widget.Box([
                                                Widget.Icon(icon_name + "-symbolic"),
                                                Widget.Label(name),
                                            ]),
                                    );

                                    self.visible = bluetooth.connected_devices.length > 0;
                                },
                                "notify::connected-devices",
                            ),
                    }),
                ],
            }),
        });

    const WifiIndicator = () =>
        Widget.Box({
            tooltipMarkup: network.wifi
            .bind("strength")
            .as((s) => "Strength: " + s.toString()),
            className: "networkWidget",
            children: [
                Widget.Icon({
                    className: "networkIcon",
                    icon: network.wifi.bind("icon_name"),
                }),
                Widget.Label({
                    className: "networkLabel",
                    label: network.wifi
                    .bind("ssid")
                    .as((ssid) => " " + ssid || " Unknown"),
                }),
            ],
        });

    const WiredIndicator = () =>
        Widget.Icon({
            className: "networkWidget",
            icon: network.wired.bind("icon_name"),
        });

    const NetworkIndicator = () =>
        Widget.Stack({
            className: "networkWidgetContainer",
            children: {
                wifi: WifiIndicator(),
                wired: WiredIndicator(),
            },
            shown: network.bind("primary").as((p) => p || "wifi"),
        });

    const focusedTitle = Widget.Label({
        className: "focusedTitle",
        xalign: 0,
        justification: "left",
        hexpand: true,
        max_width_chars: 54,
        truncate: "end",
        wrap: true,
        use_markup: true,
        label: hyprland.active.client.bind("title"),
        visible: hyprland.active.client.bind("address").as((addr) => addr !== "0x"),
    });
    const Workspaces = () =>
        Widget.EventBox({
            className: "workspaceContainerContainer",
            child: Widget.Box({
                className: "workspaceContainer",
                children: Array.from({ length: 10 }, (_, i) => i + 1).map((i) =>
                    Widget.Button({
                        className: "workspace",
                        label: "",
                        attribute: i,
                        onClicked: () => dispatch(i),
                    }),
                ),
                // remove this setup hook if you want fixed number of buttons
                setup: (self) =>
                    self.hook(hyprland, () =>
                        self.children.forEach((btn) => {
                            btn.visible = hyprland.workspaces.some(
                                (ws) => ws.id === btn.attribute,
                            );
                            if (btn.attribute == hyprland.active.workspace["id"]) {
                                btn.className = "workspace activeWorkspace";
                            } else {
                                btn.className = "workspace";
                            }
                        }),
                    ),
            }),
        });

    const volumeWidget = Widget.EventBox({
            className: "volumeWidgetContainerContainer",
            child: Widget.Box({
                className: "volumeWidgetContainer",
                children: [
                    Widget.Icon().hook(audio.speaker, (self) => {
                        const vol = audio.speaker.volume * 100;
                        const icon = [
                            [101, "overamplified"],
                            [67, "high"],
                            [34, "medium"],
                            [1, "low"],
                            [0, "muted"],
                        ].find(([threshold]) => threshold <= vol)?.[1];
                        self.className = "volumeWidgetIcon";
                        self.icon = `audio-volume-${icon}-symbolic`;
                }).hook(
                    audio.speaker,
                    self => {
                        if (Utils.exec("wpctl get-volume @DEFAULT_AUDIO_SINK@").includes("MUTED")){
                            self.icon = "audio-volume-muted-symbolic"
                        }
                    },
                    "changed"
                ),
                    Widget.Label({
                        label: audio["speaker"]
                        .bind("volume")
                        .as((v) => " " + String((v * 100).toFixed(0)) + "%"),
                    }),
                ],
            }),
    });
    
    const systrayWidget = () =>
        Widget.Box({
            className: "systrayWidget",
            children: systemtray.bind("items").as((i) =>
                i.map((item) => {
                    return Widget.Button({
                        className: "systrayItem",
                        child: Widget.Icon().bind("icon", item, "icon"),
                        tooltipMarkup: item.bind("tooltip_markup"),
                        onPrimaryClick: (_, event) => item.activate(event),
                        onSecondaryClick: (_, event) => item.openMenu(event),
                    });
                }),
            ),
        });
    const batteryWidget = Widget.Box({
        className: "batteryWidget",
        children: [
            Widget.Icon({
                className: "batteryWidgetIcon",
                icon: "battery-good-symbolic",
            }),
            Widget.Label({
                label: "Loading",
            }),
            Widget.Label({
                label: "",
                className: "batteryChargeIndicator",
            }),
        ],
    });


    

    Utils.interval(1000, ()=>{

        const script = "/home/chilly/flakes/ags/scripts/battery.sh"

        Utils.execAsync(script)
            .then(output => {
                try {
                    const batteryData = JSON.parse(output);

                    // Update icon based on percentage and charging status
                    let iconName = "battery-good-symbolic";
                    if (batteryData.charging) {
                        // iconName = "battery-good-charging-symbolic";
                        batteryWidget.children[1].className = "chargingBattery";
                        if (batteryData.percentage <= 20) {
                            iconName = "battery-level-20-charging-symbolic";
                        }else if (batteryData.percentage <= 40) {
                            iconName = "battery-level-40-charging-symbolic";
                        } else if (batteryData.percentage <= 60) {
                            iconName = "battery-level-60-charging-symbolic";
                        } else {
                            iconName = "battery-level-100-charged-symbolic";
                        }
                    } else {
                        if (batteryData.percentage <= 20) {
                            iconName = "battery-empty-symbolic";
                            batteryWidget.children[1].className = "lowBattery";
                        }else if (batteryData.percentage <= 40) {
                            iconName = "battery-level-40-symbolic";
                            batteryWidget.children[1].className = "mediumBattery";
                        } else if (batteryData.percentage <= 60) {
                            iconName = "battery-level-60-symbolic";
                            batteryWidget.children[1].className = "mediumBattery";
                        } else {
                            iconName = "battery-level-100-symbolic";
                            batteryWidget.children[1].className = "highBattery";
                        }
                    }

                    // Update widget children
                    batteryWidget.children[0].icon = iconName;
                    batteryWidget.children[1].label = ` ${batteryData.percentage}%`;
                    batteryWidget.children[2].label = ` ${(batteryData.charging ? "" : "")}`;
                } catch (e) {
                    batteryWidget.children[1].label = "Error";
                    print(`Battery script error: ${e}`);
                }
            })
            .catch(error => {
                batteryWidget.children[1].label = "Error";
                print(`Battery script execution error: ${error}`);
            });
    });

    const dateWidget = Widget.Box({
        className: "dateWidget",
        children: [
            Widget.Label({
                className: "dateWidgetIcon",
                label: " ~ 󰥔",
            }),
            Widget.Label({
                label: "Loading",
            }),
        ],
    });

    const Left = Widget.Box({
        hpack: "start",
        children: [bluetoothWidget(), NetworkIndicator(), Music(), Workspaces()],
    });
    const Right = Widget.Box({
        hpack: "end",
        children: [hyprlandClient, dateWidget, batteryWidget, volumeWidget, systrayWidget()],
    });

    const Center = Widget.Box({
        hpack: "center",
        children: [],
    });

    Utils.interval(1000, () => {
        dateWidget.children[1].label = " " + Utils.exec("date '+%A, %d %B - %I:%M %p'");
    });
    return Widget.Window({
        monitor,
        name: "bar",
        layer: "top",
        exclusivity: "normal",
        margins: [0, 5],
        class_name: "mainBar",
        anchor: ["top", "left", "right"],
        child: Widget.CenterBox({
            start_widget: Left,
            center_widget: Center,
            end_widget: Right,
        }),
    });
}

function AppLauncher(
    monitor = 0,
    { width = 300, height = 1000, spacing = 12 } = {},
) {
    let AppItem = (app) =>
        Widget.Button({
            attribute: { app },
            tooltipMarkup: app.name,
            className: "appLauncherItemContainer",
            on_clicked: () => {
                App.closeWindow("appLauncher");
                app.launch();
            },
            child: Widget.Box({
                className: "appLauncherItem",
                children: [
                    Widget.Icon({
                        className: "appLauncherItemIcon",
                        icon: app.icon_name || "",
                        size: 42,
                    }),
                    Widget.Label({
                        class_name: "title",
                        label: "  " + app.name,
                        xalign: 0,
                        vpack: "center",
                        truncate: "end",
                    }),
                ],
            }),
        });

    let applications = query("").map(AppItem);

    const input = Widget.Box({
        className: "appLauncherSearchContainer",
        children: [
            Widget.Icon({
                icon: "system-search-symbolic",
                size: 16,
                className: "appLauncherSearchIcon"
            }),
            Widget.Entry({
                hexpand: true,
                has_frame: false,
                placeholder_text: "Search...",
                className: "appLauncherSearch",
                visibility: true,
                text: "",
                // Launch first item on Enter
                on_accept: () => {
                    const results = applications.filter((app) => app.visible);
                    if (results[0]) {
                        App.toggleWindow("appLauncher");
                        results[0].attribute.app.launch();
                    }
                },
    
                on_change: ({ text }) => {
                    applications.forEach((app) => {
                        app.visible = app.attribute.app.match(text ?? "");
                    });
                },
            })
        ]
    });
    

    const list = Widget.Box({
        className: "appLauncherList",
        children: applications,
        spacing: spacing,
        vertical: true,
    });

    function repopulate() {
        applications = query("").map(AppItem);
        list.children = applications;
    }
    const appLauncher = Widget.Window({
        monitor,
        name: "appLauncher",
        layer: "overlay",
        visible: false,
        keymode: "exclusive",
        className: "appLauncherContainer",
        anchor: ["left", "top", "bottom"],
        child: Widget.Box({
            className: "appLauncher",
            vertical: true,
            children: [
                input,
                Widget.Scrollable({
                    css:
                    `min-width: ${width}px;` +
                        `min-height: ${height}px;` +
                        `padding-left: ${spacing * 2}px; ` +
                        `padding-right: ${spacing * 2}px; ` +
                        `padding-top: ${spacing * 2}px; `,
                    child: list,
                }),
            ],
            setup: (self) =>
                self.hook(App, (_, windowName, visible) => {
                    if (windowName !== "appLauncher") return;
                    // when the applauncher shows up
                    if (visible) {
                        repopulate();
                        input.text = "";
                        input.grab_focus();
                    }
                }),
        }),
    });
    appLauncher.keybind([""], "Escape", (self, event) => {
        App.toggleWindow("appLauncher");
    });

    return appLauncher;
}

App.config({
    windows: [
        // window definitions
        mainBar(0),
        AppLauncher(0),
        NotificationsPopups(0),
    ],
    style: "./config.css",
});
