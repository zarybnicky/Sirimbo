import {
  EventRegistrationCandidatesDocument,
  EventRegistrationsDocument,
  SetEventRegistrationDocument,
  type EventInstanceRegistrationFragment,
  type EventWithTrainerFragment,
} from '@/graphql/Event';
import { DialogTitle } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { formatCoupleName, formatRegistrant } from '@/ui/format';
import { EventRegistrationForm } from '@/ui/forms/EventRegistrationForm';
import { NewExternalRegistrationForm } from '@/ui/forms/NewExternalRegistrationForm';
import { Spinner } from '@/ui/Spinner';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { ChevronLeft, ChevronRight, List, Plus } from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';

type Page = 'overview' | 'candidates' | 'editor';

type Registrant = {
  id: string;
  label: string;
  personId: string | null;
  coupleId: string | null;
};

const registrantKey = (personId: string | null, coupleId: string | null) =>
  personId ? `person:${personId}` : `couple:${coupleId}`;

const registrant = (registration: EventInstanceRegistrationFragment): Registrant => ({
  id: registrantKey(registration.personId, registration.coupleId),
  label: formatRegistrant(registration),
  personId: registration.personId,
  coupleId: registration.coupleId,
});

function useRegistrationCandidates(
  instance: EventWithTrainerFragment,
  registrations: EventInstanceRegistrationFragment[],
  isManager: boolean,
) {
  const auth = useAuth();
  const [query] = useQuery({
    query: EventRegistrationCandidatesDocument,
    pause: !isManager,
  });
  const registeredPeople = new Set(
    registrations.flatMap((r) =>
      r.personId
        ? [r.personId]
        : [r.couple?.man?.id, r.couple?.woman?.id].filter((id): id is string => !!id),
    ),
  );
  const tenant = query.data?.tenant;
  const people = isManager
    ? [
        ...(tenant?.tenantMembershipsList ?? []),
        ...(tenant?.tenantTrainersList ?? []),
        ...(tenant?.tenantAdministratorsList ?? []),
      ].flatMap((membership) => (membership.person ? [membership.person] : []))
    : auth.persons;
  const couples = isManager ? (tenant?.couplesList ?? []) : auth.couples;
  const capacityLeft = instance.remainingPersonSpots ?? Number.POSITIVE_INFINITY;
  const candidates: Registrant[] = [
    ...[...new Map(people.map((person) => [person.id, person])).values()]
      .filter((person) => !registeredPeople.has(person.id))
      .map((p) => ({
        id: registrantKey(p.id, null),
        label: p.name,
        personId: p.id,
        coupleId: null,
      })),
    ...[...new Map(couples.map((c) => [c.id, c])).values()]
      .filter(
        (c) =>
          c.status === 'ACTIVE' &&
          !registeredPeople.has(c.man?.id ?? '') &&
          !registeredPeople.has(c.woman?.id ?? '') &&
          (isManager || capacityLeft >= (instance.capacityUnit === 'PEOPLE' ? 2 : 1)),
      )
      .map((couple) => ({
        id: registrantKey(null, couple.id),
        label: formatCoupleName(couple),
        personId: null,
        coupleId: couple.id,
      })),
  ]
    .filter(() => isManager || capacityLeft >= 1)
    .toSorted((a, b) => a.label.localeCompare(b.label));

  return {
    candidates,
    error: query.error,
    loading: isManager && query.fetching && !query.data,
  };
}

export function MyRegistrationsDialog({
  instance,
  isManager,
  initialRegistrationId,
}: {
  instance: EventWithTrainerFragment;
  isManager: boolean;
  initialRegistrationId?: string;
}) {
  const auth = useAuth();
  const { onSuccess } = useFormResult();

  return auth.isExternal ? (
    <NewExternalRegistrationForm instanceId={instance.id} />
  ) : (
    <RegistrationsDialogContent
      instance={instance}
      isManager={isManager}
      initialRegistrationId={initialRegistrationId}
      onClose={onSuccess}
    />
  );
}

function RegistrationsDialogContent({
  instance,
  isManager,
  initialRegistrationId,
  onClose,
}: {
  instance: EventWithTrainerFragment;
  isManager: boolean;
  initialRegistrationId?: string;
  onClose: () => void;
}) {
  const auth = useAuth();
  const [page, setPage] = React.useState<Page>();
  const [selected, setSelected] = React.useState<Registrant>();
  const [query] = useQuery({
    query: EventRegistrationsDocument,
    variables: { id: instance.id },
  });
  const setRegistration = useMutation(SetEventRegistrationDocument)[1];
  const allRegistrations = query.data?.eventInstance?.registrationsList ?? [];
  const registrations = isManager
    ? allRegistrations
    : allRegistrations.filter(
        (r) => auth.isMyPerson(r.personId) || auth.isMyCouple(r.coupleId),
      );
  const {
    candidates,
    error: candidateError,
    loading: candidatesLoading,
  } = useRegistrationCandidates(instance, allRegistrations, isManager);

  React.useEffect(() => {
    if (!query.data || page) return;
    const initialRegistration = registrations.find((r) => r.id === initialRegistrationId);
    if (initialRegistration) {
      setSelected(registrant(initialRegistration));
      setPage('editor');
    } else if (registrations.length > 1) {
      setPage('overview');
    } else if (registrations[0]) {
      setSelected(registrant(registrations[0]));
      setPage('editor');
    } else {
      setPage('candidates');
    }
  }, [initialRegistrationId, page, query.data, registrations]);

  const selectedRegistration = selected
    ? registrations.find((r) => registrantKey(r.personId, r.coupleId) === selected.id)
    : undefined;
  const i = selectedRegistration ? registrations.indexOf(selectedRegistration) : -1;
  const simpleRegistration = instance.type === 'LESSON' || instance.type === 'GROUP';

  const registerImmediately = useAsyncCallback(async (candidate: Registrant) => {
    const result = await setRegistration({
      input: {
        pInstanceId: instance.id,
        pPersonId: candidate.personId,
        pCoupleId: candidate.coupleId,
        pIsRegistered: true,
      },
    });
    if (result.error) throw result.error;
    toast.success('Přihlášení proběhlo úspěšně.');
    onClose();
  });

  const selectCandidate = (candidate: Registrant) => {
    if (simpleRegistration) {
      registerImmediately.execute(candidate);
    } else {
      setSelected(candidate);
      setPage('editor');
    }
  };

  const selectRegistration = (registrant: Registrant) => {
    setSelected(registrant);
    setPage('editor');
  };

  const changeRegistration = (n: number) => {
    const x = registrations[(i + n + registrations.length) % registrations.length];
    if (x) selectRegistration(registrant(x));
  };

  if (!query.data) {
    return query.error ? <FormError error={query.error} /> : <Spinner />;
  }

  if (page === 'overview') {
    return (
      <>
        <div className="flex items-center gap-2 pr-8">
          <DialogTitle className="grow">Přihlášky</DialogTitle>
          {candidates.length > 0 && (
            <IconButton label="Přidat přihlášku" onClick={() => setPage('candidates')}>
              <Plus />
            </IconButton>
          )}
        </div>
        <RegistrantList
          options={registrations.map(registrant)}
          searchable={registrations.length > 8}
          onSelect={selectRegistration}
        />
      </>
    );
  }

  if (page === 'candidates') {
    return (
      <>
        <div className="flex items-center gap-2 pr-8">
          {registrations.length > 1 && (
            <IconButton label="Zobrazit přihlášky" onClick={() => setPage('overview')}>
              <List />
            </IconButton>
          )}
          <DialogTitle>Koho přihlásit</DialogTitle>
        </div>
        <FormError error={candidateError || registerImmediately.error} />
        {candidatesLoading ? (
          <Spinner />
        ) : candidates.length > 0 ? (
          <RegistrantList
            autoFocus
            options={candidates}
            searchable={isManager}
            onSelect={selectCandidate}
          />
        ) : (
          <div className="text-neutral-11">Není koho dalšího přihlásit.</div>
        )}
      </>
    );
  }

  if (!selected) return null;

  return (
    <>
      <div className="flex items-center gap-2 pr-8">
        {registrations.length > 1 && (
          <IconButton label="Zobrazit přihlášky" onClick={() => setPage('overview')}>
            <List />
          </IconButton>
        )}
        {selectedRegistration ? (
          <>
            {registrations.length > 1 && (
              <IconButton
                label="Předchozí přihláška"
                onClick={() => changeRegistration(-1)}
              >
                <ChevronLeft />
              </IconButton>
            )}
            <DialogTitle className="grow text-center">
              {selected.label}
              {registrations.length > 1 && ` (${i + 1}/${registrations.length})`}
            </DialogTitle>
            {registrations.length > 1 && (
              <IconButton
                label="Následující přihláška"
                onClick={() => changeRegistration(1)}
              >
                <ChevronRight />
              </IconButton>
            )}
            {candidates.length > 0 && (
              <IconButton label="Přidat přihlášku" onClick={() => setPage('candidates')}>
                <Plus />
              </IconButton>
            )}
          </>
        ) : (
          <>
            <DialogTitle className="grow">{selected.label}</DialogTitle>
            <button
              type="button"
              className={buttonCls({ size: 'xs', variant: 'outline' })}
              onClick={() => setPage('candidates')}
            >
              Změnit
            </button>
          </>
        )}
      </div>

      <EventRegistrationForm
        key={`${selected.id}:${selectedRegistration?.id ?? 'new'}`}
        instanceId={instance.id}
        enableDetails={!simpleRegistration}
        enableNotes={!!instance.enableNotes}
        personId={selected.personId}
        coupleId={selected.coupleId}
        registration={selectedRegistration}
        lessonTrainers={query.data.eventInstance?.trainersList ?? []}
        onSaved={() => {
          if (!selectedRegistration) onClose();
        }}
        onCancelled={() => {
          const remaining = registrations.filter(
            (r) => registrantKey(r.personId, r.coupleId) !== selected.id,
          );
          if (remaining.length > 1) {
            setPage('overview');
          } else if (remaining[0]) {
            selectRegistration(registrant(remaining[0]));
          } else {
            setSelected(undefined);
            setPage('candidates');
          }
        }}
      />
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
  options,
  searchable,
  onSelect,
}: {
  autoFocus?: boolean;
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
              className={buttonCls({ display: 'listItem', variant: 'none', size: 'none' })}
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
