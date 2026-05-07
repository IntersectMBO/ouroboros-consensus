# Operational Certificate Checks

An operational certificate (OCert) delegates block-signing authority from a cold key to a hot key for a limited range of KES periods.
These checks ensure the certificate is currently valid and has not been reused improperly.

**References:**
- Agda: `Spec.OperationalCertificate` ([OperationalCertificate.lagda](../../agda-spec/src/Spec/OperationalCertificate.lagda))
- Implementation: `Ouroboros.Consensus.Protocol.Praos:doValidateKESSignature`

## KES period validity

The block's slot determines a KES period.
The certificate declares a start KES period.
The KES period must fall within the certificate's validity window: from the start period up to (but not including) start + `MaxKESEvolutions`.

**Types:**

```quint
type KESPeriod = int

type OCert = {
  hotKey: HotKey,            // operational (hot) verification key
  issueNumber: int,          // certificate issue number
  startKesPeriod: KESPeriod, // start of KES validity window
  coldKeySig: Signature      // cold key signature over (hotKey, issueNumber, startKesPeriod)
}
```

**Constants:**

```quint
const MaxKESEvolutions: int  // number of KES periods a certificate is valid for
const SlotsPerKESPeriod: int // how many slots make up one KES period
```

**Rule (RULE-OC-1):**

```quint
def kesPeriodOf(slot: Slot): KESPeriod = slot / SlotsPerKESPeriod

def validKesPeriod(slot: Slot, cert: OCert): bool = and {
  cert.startKesPeriod <= kesPeriodOf(slot),
  kesPeriodOf(slot) < cert.startKesPeriod + MaxKESEvolutions
}
```

### Example

```
MaxKESEvolutions = 62
SlotsPerKESPeriod = 129600

slot = 518400, cert.startKesPeriod = 3

kesPeriodOf(518400) = 4
validKesPeriod = (3 <= 4) and (4 < 3 + 62) = true
```

### Counter-examples

**KES period before start:**

```
slot = 259200, cert.startKesPeriod = 3

kesPeriodOf(259200) = 2
validKesPeriod = (3 <= 2) = false  // KESBeforeStart
```

**KES period expired:**

```
MaxKESEvolutions = 62
slot = 8424000, cert.startKesPeriod = 3

kesPeriodOf(8424000) = 65
validKesPeriod = (65 < 3 + 62) = (65 < 65) = false  // KESAfterEnd
```
