# Compilation de Machine Abstraites vers UXN
```
. Stage Timothée
|
+-- tests
|
+-- uxn_machine
|
+-- uxn_source
```

## Compilation de la machine virtuelle UXN
Dans le répertoire `uxn_source`, il y a le code source de la machine virtuelle UXN. Pour la compiler, suivre les instructions du `README.md`. La compilation génère 3 exécutables dans `/bin` :
- `uxnasm` : l'assembleur qui transforme le code assembleur en code binaire
- `uxncli` : machine virtuelle UXN ne gérant que l'interface clavier (terminal)
- `uxnemu` : machine virtuelle UXN complète (clavier, souris, écran, etc.)

Dans le cadre général, les exécutables `uxnasm` et `uxncli` seront suffisants. Ainsi, on peut les copier-coller dans le répertoire `uxn_machine` pour pouvoir assembler et exécuter nos codes UXN.