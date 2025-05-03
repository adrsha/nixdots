import { bind } from "astal";
import { App, Gtk, astalify, Astal } from "astal/gtk3";
import { GLib, Variable } from "astal";

export default function Calendar() {
    const C = astalify(Gtk.Calendar);
    const { TOP, RIGHT } = Astal.WindowAnchor

    const time = Variable<string>("").poll(1000, () =>
        GLib.DateTime.new_now_local().format("%H:%M - %A %e %B")!)
    return (
        <window
            name="calendarWindow"
            className="calendarWindow"
            marginRight={20}
            marginLeft={20}
            exclusivity={Astal.Exclusivity.EXCLUSIVE}
            visible={false}
            application={App}
            anchor={TOP | RIGHT}>

            <box className="calendar" spacing={10} vertical>
                <label
                    onDestroy={() => time.drop()}
                    label={time()}
                />
                <box className="container" css="padding: 0px 4px;">
                    <C hexpand />
                </box>
            </box>
        </window>
    );
}
