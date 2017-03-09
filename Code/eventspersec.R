DT <- data.table(name=colnames(train_g2_L16B_min))
DT[name %like% "Request"]
# 1:              McRrcConnectionRequest
# 2:                RrcConnectionRequest
# 3:        S1InitialContextSetupRequest
# 4:           S1UeContextReleaseRequest
# 5:                 McX2HandoverRequest
# 6:                   X2HandoverRequest
# 7:        X2HandoverRequestAcknowledge
# 8:                  S1ErabSetupRequest
# 9: RrcConnectionReEstablishmentRequest

DT[name %like% "Handover"]
# 1:            McX2HandoverRequest
# 2:              X2HandoverRequest
# 3:   X2HandoverRequestAcknowledge
# 4: McX2HandoverPreparationFailure
# 5:   X2HandoverPreparationFailure

DT[name %like% "ReEstablish"]
# 1:    ProcRrcConnectionReEstablishment
# 2:              ReEstablishmentAttempt
# 3:  RrcConnectionReEstablishmentReject
# 4: RrcConnectionReEstablishmentRequest

g2_L16B_min$S1InitialUeMessage

