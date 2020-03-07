namespace Fable.Localization

open Fable
open Fable.React.Standard
open Fable.React.Helpers

//type LocalizedStringComponentProps<'T> = {
//    GetString : (string * 'T) -> string
//    Key : string
//    //Args :
//    Reevaluate : IEvent<unit>
//}

//type LocalizedStringComponentState = {
//    Language : string
//}


//type LocalizedStringComponent<'TArgs>(initProps:LocalizedStringComponentProps) as this =
//    inherit React.Component<LocalizedStringComponentProps, LocalizedStringComponentState>(initProps)
//    do this.setInitState({ Text = initProps.GetString() })

//    let onLanguageChanged : Handler<unit> = Handler<string>(fun _ _ -> this.setState(fun _s p -> { Language = l }))

//    do initProps.Reevaluate.AddHandler onLanguageChanged

//    override this.componentWillUnmount() =
        

//    override this.render() = str (this.state.Text)
