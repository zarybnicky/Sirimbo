import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import React from 'react';
import { ChangeCoursesForm } from './change-courses-form';
import { ChangeFoldersForm } from './change-folders-form';
import { ChangeLoginForm } from './change-login-form';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { CohortComparisonForm } from './cohort-comparison-form';
import { PersonComparisonForm } from './person-comparison-form';
import { PersonComparisonPreview } from './person-comparison-preview';
import { parseAsBoolean, useQueryState } from 'nuqs';

export function StarletImporter() {
  const [comparisonPreview, setComparisonPreview] = useQueryState(
    'comparison',
    parseAsBoolean.withDefault(false),
  );
  const token = useAtomValue(starletTokenAtom);
  const { auth, folders, seasons } = useAtomValue(starletSettingsAtom);

  return (
    <div className="prose prose-accent">
      <h2 className="flex justify-between">
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

      <h2 className="flex justify-between">
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
        Sezóny: {seasons.map(({ name }) => name).join(', ')}
        <br />
        Složky: {folders.map(({ name }) => name).join(', ')}
      </p>

      <h2 className="flex justify-between">
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

      <h2 className="flex justify-between">
        <span>4. Studenti</span>
        <button
          type="button"
          role="switch"
          aria-checked={comparisonPreview}
          className="not-prose flex items-center self-center gap-2 rounded-sm text-sm font-normal focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-9"
          onClick={() => setComparisonPreview((v) => !v)}
        >
          <span>Nové porovnání</span>
          <span
            aria-hidden
            className="inline-flex h-5 w-9 items-center rounded-full bg-accent-7 p-0.5"
          >
            <span
              className={`size-4 rounded-full bg-neutral-1 motion-safe:transition-transform ${comparisonPreview ? 'translate-x-4' : 'translate-x-0'}`}
            />
          </span>
        </button>
      </h2>

      {comparisonPreview ? <PersonComparisonPreview /> : <PersonComparisonForm />}
    </div>
  );
}
