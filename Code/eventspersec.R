DT <- data.table(name=colnames(train_g2_L16B_min))

DT[name %like% "Request"]
# 1:              McRrcConnectionRequest (cor~1)
# 2:                RrcConnectionRequest (cor~1)
# 3:        S1InitialContextSetupRequest (cor~1)
# 4:           S1UeContextReleaseRequest (cor~1)
# 5:                 McX2HandoverRequest
# 6:                   X2HandoverRequest
# 7:        X2HandoverRequestAcknowledge
# 8:                  S1ErabSetupRequest (0)
# 9: RrcConnectionReEstablishmentRequest (0)

DT[name %like% "Handover"]
# 1:            McX2HandoverRequest
# 2:              X2HandoverRequest
# 3:   X2HandoverRequestAcknowledge
# 4: McX2HandoverPreparationFailure
# 5:   X2HandoverPreparationFailure

DT[name %like% "ReEstablish"]
# 1:    ProcRrcConnectionReEstablishment (0)
# 2:              ReEstablishmentAttempt (0)
# 3:  RrcConnectionReEstablishmentReject (0)
# 4: RrcConnectionReEstablishmentRequest (0)

DT[name %like% "Reject"]
# 1:              McRrcConnectionReject
# 2:                RrcConnectionReject
# 3:                    Srb1SetupReject
# 4:                  UeAdmissionReject (0)
# 5: RrcConnectionReEstablishmentReject (0)

n <- "ProcRrcConnectionReEstablishment"
unique(g2_L16B_min[,n])
unique(g2_extract[,n])

g2_L16B_min$ProcRrcConnectionReEstablishment
g2_extract$ProcRrcConnectionReEstablishment

# check correlation between RrcConnectionSetupComplete
cor(g2_L16B_min$McRrcConnectionReject, g2_L16B_min$RrcConnectionSetupComplete)
