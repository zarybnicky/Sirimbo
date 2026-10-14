import {
  type EventInstanceRegistrationFragment,
  EventRegistrationCandidatesDocument,
  EventRegistrationDialogDocument,
  type EventRegistrationDialogQuery,
  SetEventRegistrationDocument,
} from '@/graphql/Event';
import { DialogTitle } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { formatCoupleName, formatRegistrant } from '@/ui/format';
import { EventRegistrationForm } from '@/ui/forms/EventRegistrationForm';
import { NewExternalRegistrationForm } from '@/ui/forms/NewExternalRegistrationForm';
import { Spinner } from '@/ui/Spinner';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { ChevronLeft, ChevronRight, List, Plus } from 'lucide-react';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';

type Registrant = {
  id: string;
  label: string;
  personId: string | null;
  coupleId: string | null;
};

type Selection =
  | { kind: 'overview' | 'candidates' }
  | { kind: 'registration'; id: string }
  | { kind: 'candidate'; registrant: Registrant };

type RegistrationInstance = NonNullable<EventRegistrationDialogQuery['instance']>;

const registrant = (registration: EventInstanceRegistrationFragment): Registrant => ({
  id: registration.id,
  label: formatRegistrant(registration),
  personId: registration.personId,
  coupleId: registration.coupleId,
});

function useRegistrationCandidates(
  instance: RegistrationInstance,
  isManager: boolean,
  enabled: boolean,
) {
  const auth = useAuth();
  const [query] = useQuery({
    query: EventRegistrationCandidatesDocument,
    pause: !isManager || !enabled,
  });
  const registeredPeople = new Set(
    instance.registrationsList.flatMap((r) =>
      r.personId
        ? [r.personId]
        : [r.couple?.man?.id, r.couple?.woman?.id].filter((id): id is string => !!id),
    ),
  );
  const people = isManager ? (query.data?.people?.nodes ?? []) : auth.persons;
  const couples = isManager
    ? (query.data?.tenant?.couplesList ?? [])
    : [...new Map(auth.couples.map((c) => [c.id, c])).values()].filter(
        (c) => c.status === 'ACTIVE',
      );
  const capacityLeft =
    instance.registrationInfo?.remainingCapacity ?? Number.POSITIVE_INFINITY;
  const candidates: Registrant[] = [
    ...people
      .filter((x) => !registeredPeople.has(x.id))
      .map((p) => ({
        id: `person:${p.id}`,
        label: p.name,
        personId: p.id,
        coupleId: null,
      })),
    ...couples
      .filter(
        (c) =>
          !registeredPeople.has(c.man?.id ?? '') &&
          !registeredPeople.has(c.woman?.id ?? '') &&
          (isManager || capacityLeft >= (instance.capacityUnit === 'PEOPLE' ? 2 : 1)),
      )
      .map((couple) => ({
        id: `couple:${couple.id}`,
        label: formatCoupleName(couple),
        personId: null,
        coupleId: couple.id,
      })),
  ]
    .filter(() => isManager || (!instance.isLocked && capacityLeft >= 1))
    .toSorted((a, b) => a.label.localeCompare(b.label));

  return {
    candidates,
    error: query.error,
    loading: isManager && query.fetching && !query.data,
  };
}

export function MyRegistrationsDialog({
  instanceId,
  initialRegistrationId,
}: {
  instanceId: string;
  initialRegistrationId?: string;
}) {
  const auth = useAuth();
  const [query] = useQuery({
    query: EventRegistrationDialogDocument,
    variables: { id: instanceId },
    pause: auth.isExternal,
  });

  if (auth.isExternal) return <NewExternalRegistrationForm instanceId={instanceId} />;
  if (!query.data) return query.error ? <FormError error={query.error} /> : <Spinner />;
  if (!query.data.instance) return <FormError error="Termín není dostupný." />;

  return (
    <RegistrationsDialogContent
      instance={query.data.instance}
      initialRegistrationId={initialRegistrationId}
    />
  );
}

function RegistrationsDialogContent({
  instance,
  initialRegistrationId,
}: {
  instance: RegistrationInstance;
  initialRegistrationId?: string;
}) {
  const auth = useAuth();
  const isManager = auth.canManageInstance(instance);
  const { onSuccess: onClose } = useFormResult();
  const registrations = isManager
    ? instance.registrationsList
    : instance.registrationsList.filter(
        (r) => auth.isMyPerson(r.personId) || auth.isMyCouple(r.coupleId),
      );
  const [selection, setSelection] = React.useState<Selection>(() => {
    const initial =
      registrations.find((r) => r.id === initialRegistrationId) ??
      (registrations.length === 1 ? registrations[0] : undefined);
    return initial
      ? { kind: 'registration', id: initial.id }
      : { kind: registrations.length > 0 ? 'overview' : 'candidates' };
  });
  const registration =
    selection.kind === 'registration'
      ? registrations.find((r) => r.id === selection.id)
      : undefined;
  const selected = registration
    ? registrant(registration)
    : selection.kind === 'candidate'
      ? selection.registrant
      : undefined;
  const page = selected
    ? 'editor'
    : selection.kind === 'candidates'
      ? 'candidates'
      : 'overview';
  const {
    candidates,
    error: candidateError,
    loading: candidatesLoading,
  } = useRegistrationCandidates(instance, isManager, page === 'candidates');
  const [setRegistrationResult, setRegistration] = useMutation(
    SetEventRegistrationDocument,
  );
  const i = registration ? registrations.indexOf(registration) : -1;
  const canCycle = !!registration && registrations.length > 1;
  const simpleRegistration = instance.type === 'LESSON' || instance.type === 'GROUP';

  const selectCandidate = async (candidate: Registrant) => {
    if (!simpleRegistration) {
      setSelection({ kind: 'candidate', registrant: candidate });
      return;
    }
    const result = await setRegistration({
      input: {
        pInstanceId: instance.id,
        pPersonId: candidate.personId,
        pCoupleId: candidate.coupleId,
        pIsRegistered: true,
      },
    });
    if (result.error) return;
    toast.success('Přihlášení proběhlo úspěšně.');
    onClose();
  };

  const changeRegistration = (offset: number) => {
    const next =
      registrations[(i + offset + registrations.length) % registrations.length];
    if (next) setSelection({ kind: 'registration', id: next.id });
  };

  return (
    <>
      <div className="flex items-center gap-2 pr-8">
        {page !== 'overview' && registrations.length > 0 && (
          <IconButton
            label="Zobrazit přihlášky"
            onClick={() => setSelection({ kind: 'overview' })}
          >
            <List />
          </IconButton>
        )}
        {canCycle && (
          <IconButton label="Předchozí přihláška" onClick={() => changeRegistration(-1)}>
            <ChevronLeft />
          </IconButton>
        )}
        <DialogTitle className="grow">
          {selected?.label ?? (page === 'candidates' ? 'Koho přihlásit' : 'Přihlášky')}
          {canCycle && ` (${i + 1}/${registrations.length})`}
        </DialogTitle>
        {canCycle && (
          <IconButton label="Následující přihláška" onClick={() => changeRegistration(1)}>
            <ChevronRight />
          </IconButton>
        )}
        {(page === 'overview' || registration) &&
          (isManager || candidates.length > 0) && (
            <IconButton
              label="Přidat přihlášku"
              onClick={() => setSelection({ kind: 'candidates' })}
            >
              <Plus />
            </IconButton>
          )}
        {selected && !registration && (
          <button
            type="button"
            className={buttonCls({ size: 'xs', variant: 'outline' })}
            onClick={() => setSelection({ kind: 'candidates' })}
          >
            Změnit
          </button>
        )}
      </div>

      {page === 'overview' ? (
        <RegistrantList
          options={registrations.map(registrant)}
          searchable={registrations.length > 8}
          onSelect={(option) => setSelection({ kind: 'registration', id: option.id })}
        />
      ) : page === 'candidates' ? (
        <>
          <FormError error={candidateError || setRegistrationResult.error} />
          {candidatesLoading ? (
            <Spinner />
          ) : candidates.length > 0 ? (
            <RegistrantList
              autoFocus
              options={candidates}
              searchable={isManager}
              disabled={setRegistrationResult.fetching}
              onSelect={selectCandidate}
            />
          ) : (
            <div className="text-neutral-11">Není koho dalšího přihlásit.</div>
          )}
        </>
      ) : (
        selected && (
          <EventRegistrationForm
            key={`${selected.id}:${registration?.id ?? 'new'}`}
            instance={instance}
            registrant={registration ?? selected}
            onSaved={() => {
              if (!registration) onClose();
            }}
            onCancelled={() => {
              const remaining = registrations.filter((r) => r.id !== registration?.id);
              setSelection(
                remaining.length > 1
                  ? { kind: 'overview' }
                  : remaining[0]
                    ? { kind: 'registration', id: remaining[0].id }
                    : { kind: 'candidates' },
              );
            }}
          />
        )
      )}
    </>
  );
}

function IconButton({
  label,
  children,
  ...props
}: React.ButtonHTMLAttributes<HTMLButtonElement> & {
  label: string;
}) {
  return (
    <button
      type="button"
      className={buttonCls({ size: 'icon', variant: 'outline' })}
      aria-label={label}
      {...props}
    >
      {children}
    </button>
  );
}

const registrantSearchFields: 'label'[] = ['label'];

function RegistrantList({
  autoFocus,
  disabled,
  options,
  searchable,
  onSelect,
}: {
  autoFocus?: boolean;
  disabled?: boolean;
  options: Registrant[];
  searchable: boolean;
  onSelect: (registrant: Registrant) => void;
}) {
  const [search, setSearch] = React.useState('');
  const filtered = useFuzzySearch(options, registrantSearchFields, search);

  return (
    <div className="scrollbar max-h-[60dvh] overflow-y-auto overscroll-contain">
      {searchable && (
        <TextField
          autoFocus={autoFocus}
          type="search"
          className="sticky top-0 z-10 bg-neutral-1"
          inputClassName="bg-neutral-1"
          placeholder="Vyhledat..."
          value={search}
          onChange={(event) => setSearch(event.currentTarget.value)}
        />
      )}
      <ul className="overflow-hidden rounded-md border border-neutral-6 divide-y divide-neutral-5">
        {filtered.map((option) => (
          <li key={option.id}>
            <button
              type="button"
              disabled={disabled}
              className={buttonCls({
                display: 'listItem',
                variant: 'none',
                size: 'none',
              })}
              onClick={() => onSelect(option)}
            >
              {option.label}
            </button>
          </li>
        ))}
        {filtered.length === 0 && <li>Nic jsme nenašli.</li>}
      </ul>
    </div>
  );
}
