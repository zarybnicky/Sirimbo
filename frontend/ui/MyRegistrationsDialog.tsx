import {
  type EventRegistrationFragment,
  EventRegistrationCandidatesDocument,
  EventRegistrationDialogDocument,
  type EventRegistrationDialogQuery,
  SetEventRegistrationDocument,
} from '@/graphql/Event';
import { DialogTitle } from '@/ui/dialog';
import { EventLoginPrompt } from '@/ui/EventLoginPrompt';
import { TextField } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { formatCoupleName, formatRegistrant } from '@/ui/format';
import { EventRegistrationForm } from '@/ui/forms/EventRegistrationForm';
import { Spinner } from '@/ui/Spinner';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { compareText } from '@/lib/compare';
import { ChevronLeft, ChevronRight, List, Plus } from 'lucide-react';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { EventSignupForm } from '@/ui/forms/EventSignupForm.tsx';

type Registrant = {
  id: string;
  label: string;
  personId: string | null;
  coupleId: string | null;
};

type Selection =
  | { kind: 'overview' | 'candidates' | 'new-person' }
  | { kind: 'registration'; id: string }
  | { kind: 'candidate'; registrant: Registrant };

type RegistrationEvent = NonNullable<EventRegistrationDialogQuery['instance']>;

const registrant = (registration: EventRegistrationFragment): Registrant => ({
  id: registration.id,
  label: formatRegistrant(registration),
  personId: registration.personId,
  coupleId: registration.coupleId,
});

function useRegistrationCandidates(event: RegistrationEvent, enabled: boolean) {
  const auth = useAuth();
  const isManager = auth.canManageInstance(event);

  const [query] = useQuery({
    query: EventRegistrationCandidatesDocument,
    pause: !isManager || !enabled,
  });
  const registeredPeople = new Set(
    event.registrationsList.flatMap((r) =>
      r.personId
        ? [r.personId]
        : [r.couple?.man?.id, r.couple?.woman?.id].filter((id): id is string => !!id),
    ),
  );

  const people = (isManager ? (query.data?.people?.nodes ?? []) : auth.persons)
    .map((person) => ({
      ...person,
      isExternal: !person.isMember && !person.isTrainer && !person.isAdmin,
    }));
  const internalPersonIds = new Set(people
    .filter((person) => !person.isExternal)
    .map((person) => person.id));
  const couples = isManager
    ? (query.data?.tenant?.couplesList ?? [])
    : [...new Map(auth.couples.map((c) => [c.id, c])).values()].filter(
        (c) => c.status === 'ACTIVE',
      );

  const capacityLeft =
    event.registrationInfo?.remainingCapacity ?? Number.POSITIVE_INFINITY;
  const candidates = [
    ...people
      .filter((x) => !registeredPeople.has(x.id))
      .map((person) => ({
        id: `person:${person.id}`,
        label: person.name + (person.isExternal ? ' (Externí)' : ''),
        isExternal: person.isExternal,
        personId: person.id,
        coupleId: null,
      })),
    ...couples
      .filter(
        (c) =>
          !registeredPeople.has(c.man?.id ?? '') &&
          !registeredPeople.has(c.woman?.id ?? '') &&
          (isManager || capacityLeft >= (event.capacityUnit === 'PEOPLE' ? 2 : 1)),
      )
      .map((couple) => ({
        id: `couple:${couple.id}`,
        label: formatCoupleName(couple),
        isExternal: ![couple.man?.id, couple.woman?.id].some((id) => !!id && internalPersonIds.has(id)),
        personId: null,
        coupleId: couple.id,
      })),
  ]
    .filter(() => isManager || (!event.isLocked && capacityLeft >= 1))
    .toSorted((a, b) => {
      const aMine = auth.isMyPerson(a.personId) || auth.isMyCouple(a.coupleId);
      const bMine = auth.isMyPerson(b.personId) || auth.isMyCouple(b.coupleId);
      return Number(a.isExternal) - Number(b.isExternal) ||
        Number(bMine) - Number(aMine) || compareText(a.label, b.label);
    });

  return {
    candidates,
    error: query.error,
    loading: isManager && query.fetching && !query.data,
  };
}

export function MyRegistrationsDialog({
  instanceId,
  initialRegistrationId,
  initialPage,
}: {
  instanceId: string;
  initialRegistrationId?: string;
  initialPage?: 'overview' | 'candidates' | 'new-person';
}) {
  const [query] = useQuery({
    query: EventRegistrationDialogDocument,
    variables: { id: instanceId },
  });

  if (!query.data) return query.error ? <FormError error={query.error} /> : <Spinner />;
  if (!query.data.instance) return <FormError error="Termín není dostupný." />;

  return (
    <RegistrationsDialogContent
      instance={query.data.instance}
      initialRegistrationId={initialRegistrationId}
      initialPage={initialPage}
    />
  );
}

function RegistrationsDialogContent({
  instance,
  initialRegistrationId,
  initialPage,
}: {
  instance: RegistrationEvent;
  initialRegistrationId?: string;
  initialPage?: 'overview' | 'candidates' | 'new-person';
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
    if (initialPage) return { kind: initialPage };
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
  const {
    candidates,
    error: candidateError,
    loading: candidatesLoading,
  } = useRegistrationCandidates(instance, selection.kind === 'candidates');
  const page = selected
    ? 'editor'
    : selection.kind === 'candidates'
      ? !candidatesLoading && !candidateError && candidates.length === 0
        ? 'new-person'
        : 'candidates'
      : selection.kind === 'new-person'
        ? 'new-person'
        : 'overview';
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
        eventId: instance.id,
        personId: candidate.personId,
        coupleId: candidate.coupleId,
        isRegistered: true,
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
        {page === 'new-person' && candidates.length > 0 && (
          <IconButton
            label="Zpět na výběr osoby"
            onClick={() => setSelection({ kind: 'candidates' })}
          >
            <ChevronLeft />
          </IconButton>
        )}
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
          {selected?.label ??
            (page === 'candidates'
              ? 'Koho přihlásit'
              : page === 'new-person'
                ? candidates.length > 0 || registrations.length > 0
                  ? 'Nová osoba'
                  : 'Přihláška'
                : 'Přihlášky')}
          {canCycle && ` (${i + 1}/${registrations.length})`}
        </DialogTitle>
        {canCycle && (
          <IconButton label="Následující přihláška" onClick={() => changeRegistration(1)}>
            <ChevronRight />
          </IconButton>
        )}
        {page === 'candidates' && (
          <button
            type="button"
            className={buttonCls({ size: 'sm', variant: 'outline' })}
            onClick={() => setSelection({ kind: 'new-person' })}
          >
            <Plus />
            Nová osoba
          </button>
        )}
        {(page === 'overview' || registration) && (
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
      ) : page === 'new-person' ? (
        <>
          <EventLoginPrompt instanceId={instance.id} />
          <EventSignupForm instance={instance} />
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
