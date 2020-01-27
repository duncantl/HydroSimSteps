     # Inline the values of LookupyQ and LookupyTa into QLookup and TaLookup body's expression.
     # Assumes both functions have no {} around the body, but each body is just a single call.
    b = body(QLookup)
    if(is.symbol(b[[2]])) {
        b[[2]] = LookupyQ
        body(QLookup) = b
    }

    b = body(TaLookup)
    if(is.symbol(b[[2]])) {
        b[[2]] = LookupyTa
        body(TaLookup) = b
    }



formals(SpringDeltaVc)$springcoeff = springcoeff
formals(WinterDeltaVc)$wintercoeff = wintercoeff
formals(SpringDeltaVc)$bin = formals(WinterDeltaVc)$bin = bin
#formals(SpringDeltaVc)["springcoeff", "bin"] = list(springcoeff, bin)


formals(benefit)$distance = distance

formals(ReleaseTemp)$LookupTablTc = LookupTableTc
formals(ReleaseTemp)$LookupTablTw = LookupTableTw
formals(ReleaseTemp)$greaterTc = greaterTc
formals(ReleaseTemp)$greaterTw = greaterTw
formals(ReleaseTemp)$Vc = Vc
formals(ReleaseTemp)$Vw = Vw



