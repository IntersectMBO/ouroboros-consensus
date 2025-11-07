I need to address (peras 4):

+    -- https://github.com/kapralVV/Unique/issues/11
+    , Unique:hashable

Remove commit about feature flags from Peras 4

+++ b/ouroboros-consensus/test/storage-test/Test/Ouroboros/Storage/PerasCertDB/StateMachine.hs
@@ -113,7 +113,15 @@ instance StateModel Model where
 
   precondition (Model model) = \case
     OpenDB -> not model.open
-    _ -> model.open
+    action ->
+      model.open && case action of
+        CloseDB -> True
+        -- Do not add equivocating certificates.
+        AddCert cert -> all p model.certs
+         where
+          p cert' = getPerasCertRound cert /= getPerasCertRound cert' || cert == cert'
+        GetWeightSnapshot -> True
+        GarbageCollect _slot -> True
