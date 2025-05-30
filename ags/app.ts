import { App } from "astal/gtk3"
import style from "./style.scss"
import Bar   from "./widget/Bar"
import {MediaDetailsWindow} from "./widget/Bar"
import Calendar from "./widget/Calendar"

App.start({
    css: style,
    main() {
      App.get_monitors().map(Bar)
      MediaDetailsWindow()
      Calendar()
    },
})
