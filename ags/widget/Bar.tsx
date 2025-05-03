import { App } from "astal/gtk3"
import { Variable, GLib, bind } from "astal"
import { Astal, Gtk, Gdk } from "astal/gtk3"
import Hyprland from 'gi://AstalHyprland';
import Mpris from 'gi://AstalMpris';
import Wp from 'gi://AstalWp';
import Network from 'gi://AstalNetwork';
import Bluetooth from 'gi://AstalBluetooth';
import Tray from 'gi://AstalTray';

export const showDetails = Variable(false)

function lengthStr(length: number) {
    const min = Math.floor(length / 60)
    const sec = Math.floor(length % 60)
    const sec0 = sec < 10 ? "0" : ""
    return `${min}:${sec0}${sec}`
}

function SysTray() {
    const tray = Tray.get_default()
    return <box className="SysTray">
        {bind(tray, "items").as(items => items.map(item => (
            <menubutton
                tooltipMarkup={bind(item, "tooltipMarkup")}
                usePopover={false}
                actionGroup={bind(item, "actionGroup").as(ag => ["dbusmenu", ag])}
                menuModel={bind(item, "menuModel")}>
                <icon gicon={bind(item, "gicon")} />
            </menubutton>
        )))}
    </box>
}

function NetworkPC() {
    const bluetooth = Bluetooth.get_default()
    const network = Network.get_default()
    const wifi = bind(network, "wifi")
    const eth = bind(network, "wired")

    // Create a reactive binding for Bluetooth state that updates when dependencies change
    const bluetoothWidget = Variable.derive(
        [
            bind(bluetooth, "isPowered"),
            bind(bluetooth, "isConnected"),
            bind(bluetooth, "devices")
        ],
        (isPowered, isConnected, devices) => {
            const connectedDevices = devices.filter(device => device.connected);
            const deviceName = connectedDevices.length > 0 ? connectedDevices[0].name : "Connected";

            if (!isPowered) {
                return <icon icon="bluetooth-disabled-symbolic" />;
            } else if (!isConnected) {
                return <icon icon="bluetooth-symbolic" />;
            } else {
                return (
                    <box className="bluetooth">
                        <icon icon="bluetooth-symbolic" />
                        <label label={deviceName} />
                    </box>
                );
            }
        }
    );

    return <box className="networkPC">
        <box visible={wifi.as(Boolean)}>
            {wifi.as(wifi => wifi && (
                <box
                    tooltipText={bind(wifi, "ssid").as(ssid =>
                        `WiFi: ${ssid || "Not Connected"}\nStrength: ${Math.round((wifi.strength || 0) * 100)}%`
                    )}>
                    <icon
                        className={`Wifi ${bind(wifi, "strength").as(s => s > 0 ? "connected" : "disconnected")}`}
                        icon={bind(wifi, "iconName")}
                    />
                    <label label={bind(wifi, "ssid").as(String)} />
                </box>
            ))}
        </box>
        <box visible={eth.as(Boolean)}>
            {eth.as(eth => {
                return eth && (
                    <icon
                        tooltipText={bind(eth, "state").as(state =>
                            state === 100 ? "Ethernet: Connected" : "Ethernet: Disconnected"
                        )}
                        className={`Ethernet ${bind(eth, "state").as(s => s === 100 ? "connected" : "disconnected")}`}
                        icon={bind(eth, "iconName")}
                    />
                )
            })}
        </box>

        {/* Use the derived variable directly */}
        {bluetoothWidget()}
    </box>
}

function AudioSlider() {
    const speaker = Wp.get_default()?.audio.defaultSpeaker!
    return <box className="AudioSlider" css="min-width: 140px">
        <icon icon={bind(speaker, "volumeIcon")} />
        <slider
            hexpand
            onDragged={({ value }) => speaker.volume = value}
            value={bind(speaker, "volume")}
        />
    </box>
}

// function BatteryLevel() {
//     const bat = Battery.get_default()
//     return <box className="Battery" visible={bind(bat, "isPresent")}>
//         <icon icon={bind(bat, "batteryIconName")} />
//         <label label={bind(bat, "percentage").as(p =>
//             `${Math.floor(p * 100)} %`
//         )} />
//     </box>
// }

function MediaComponent() {
    const mpris = Mpris.get_default()

    return bind(mpris, "players").as(players => {
        const player = players[0]
        if (!player) return <label label="Nothing Playing" />

        const playerIcon = bind(player, "entry").as(e =>
            Astal.Icon.lookup_icon(e) ? e : "audio-x-generic-symbolic")

        return <box>
            <icon icon={playerIcon} />
            <button className="coverButton" onClicked={() => {
                showDetails.set(!showDetails.get())
            }} >
                <label label={bind(player, "title").as(String)} />
                {// <box
                    //     className="cover"
                    //     valign={Gtk.Align.CENTER}
                    //     css={bind(player, "coverArt").as(cover => 
                    //         `background-image: url('${cover}'); background-size: cover; background-position: center;`
                    //     )}
                    // />
                }
            </button>
        </box>
    })
}

export function MediaDetailsWindow() {
    const mpris = Mpris.get_default()


    return <window
        className="mediaPop"
        anchor={Astal.WindowAnchor.TOP | Astal.WindowAnchor.LEFT}
        exclusivity={Astal.Exclusivity.NONE}
        marginLeft={20}
        focusable
        layer={Astal.Layer.OVERLAY}
        visible={bind(showDetails)}>

        <box className="detailsBox" vertical>
            {bind(mpris, "players").as(players => {
                const player = players[0]

                const position = bind(player, "position").as(p => player.length > 0
                    ? p / player.length : 0)

                const playIcon = bind(player, "playbackStatus").as(s =>
                    s === Mpris.PlaybackStatus.PLAYING
                        ? "" : ""
                )

                const playerIcon = bind(player, "entry").as(e =>
                    Astal.Icon.lookup_icon(e) ? e : "audio-x-generic-symbolic")


                if (!player) return <label label="Nothing Playing" />
                return <box vertical>
                    <box
                        className="cover"
                        valign={Gtk.Align.CENTER}
                        css={bind(player, "coverArt").as(cover =>
                            `min-width: 50px; min-height: 150px; background-image: url('${cover}'); background-size: cover; background-position: center;`
                        )}
                    />
                    <box className="playerIcon" halign={Gtk.Align.END} ><icon icon={playerIcon} /></box>
                    <label label={bind(player, "title").as(String)} />
                    <label label={bind(player, "artist").as(String)} />
                    <slider
                        visible={bind(player, "length").as(l => l > 0)}
                        onDragged={({ value }) => player.position = value * player.length}
                        value={position}
                    />
                    <box className="player" halign={Gtk.Align.CENTER} >
                        <button onClicked={() => player.previous()}><label label="󰒮" /></button>
                        <button onClicked={() => player.play_pause()}><label label={playIcon} /></button>
                        <button onClicked={() => player.next()}><label label="󰒭" /></button>
                    </box>
                    <label
                        hexpand
                        className="position"
                        halign={Gtk.Align.START}
                        visible={bind(player, "length").as(l => l > 0)}
                        label={bind(player, "position").as(lengthStr)}
                    />
                </box>
            })}
        </box>
    </window>
}

function Workspaces() {
    const hypr = Hyprland.get_default()
    return <box className="Workspaces">
        {bind(hypr, "workspaces").as(wss => wss
            .filter(ws => !(ws.id >= -99 && ws.id <= -2))
            .sort((a, b) => a.id - b.id)
            .map(ws => (
                <button
                    className={bind(hypr, "focusedWorkspace").as(fw =>
                        ws === fw ? "focused" : "")}
                    onClicked={() => ws.focus()}>
                    {ws.id}
                </button>
            ))
        )}
    </box>
}

function FocusedClient() {
    const hypr = Hyprland.get_default()
    const focused = bind(hypr, "focusedClient")

    return <box className="Focused" visible={focused.as(Boolean)}>
        {focused.as(client =>
            client && <label label={bind(client, "title").as(String)} />
        )}
    </box>
}

function Time({ format = "%H:%M - %A" }) {
    const time = Variable<string>("").poll(1000, () =>
        GLib.DateTime.new_now_local().format(format)!)

    return (
        <button className="Time" onClick={() => App.toggle_window("calendarWindow")}>
            <label
                onDestroy={() => time.drop()}
                label={time()}
            />
        </button>
    )
}

export default function Bar(monitor: Gdk.Monitor) {
    const { TOP, LEFT, RIGHT } = Astal.WindowAnchor

    return <>
        <window
            className="Bar"
            gdkmonitor={monitor}
            marginRight={20}
            marginLeft={20}
            marginBottom={-20}
            exclusivity={Astal.Exclusivity.EXCLUSIVE}
            anchor={TOP | LEFT | RIGHT}>
            <centerbox>
                <box hexpand halign={Gtk.Align.START}>
                    <Workspaces />
                    <MediaComponent />
                </box>
                <box>
                </box>
                <box hexpand halign={Gtk.Align.END}>
                    <AudioSlider />
                    <SysTray />
                    <NetworkPC />
                    <Time />
                </box>
            </centerbox>
        </window>
    </>
}
