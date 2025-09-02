import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import React from 'react';
import { ChangeCoursesForm } from './change-courses-form';
import { ChangeFoldersForm } from './change-folders-form';
import { ChangeLoginForm } from './change-login-form';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { CohortComparisonForm } from './cohort-comparison-form';
import { PersonComparisonForm } from './person-comparison-form';

export function StarletImporter() {
  const token = useAtomValue(starletTokenAtom);
  const { auth, folders, seasons } = useAtomValue(starletSettingsAtom);

  return (
    <div className="prose">
      <h2 className="flex justify-between my-0">
        <span>1. Přihlašovací údaje</span>
        <Dialog>
          <DialogTrigger size="sm" text="Nahradit přihlašovací údaje" />
          <DialogContent>
            <ChangeLoginForm />
          </DialogContent>
        </Dialog>
      </h2>

      <p>
        {auth ? (
          token ? (
            // eslint-disable-next-line unicorn/no-nested-ternary
            token.auth_ok ? (
              <>
                Přihlášen v evidenci jako <b>{token.login}</b>
              </>
            ) : (
              'Neplatné přihlašovací údaje do evidence'
            )
          ) : (
            'Pokouším se přihlásit do evidence...'
          )
        ) : (
          'Přihlašovací údaje do evidence nevyplněny'
        )}
      </p>

      <h2 className="flex justify-between my-0">
        <span>2. Sezóny a složky</span>
        {token?.auth_ok && (
          <Dialog>
            <DialogTrigger size="sm" text="Upravit výběr" />
            <DialogContent>
              <ChangeFoldersForm />
            </DialogContent>
          </Dialog>
        )}
      </h2>

      <p>
        Sezóny: {seasons.map((x) => x[1]).join(', ')}
        <br />
        Složky: {folders.map((x) => x[1]).join(', ')}
      </p>

      <h2 className="flex justify-between my-0">
        <span>3. Kurzy</span>
        {token?.auth_ok && (
          <Dialog>
            <DialogTrigger size="sm" text="Upravit výběr" />
            <DialogContent>
              <ChangeCoursesForm />
            </DialogContent>
          </Dialog>
        )}
      </h2>

      <CohortComparisonForm />

      <h2 className="flex justify-between my-0">
        <span>4. Studenti</span>
      </h2>

      <PersonComparisonForm />
    </div>
  );
}
