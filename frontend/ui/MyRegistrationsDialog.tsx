import {
  EventFormOptionsDocument,
  type EventRegistrationFragment,
  EventRegistrationsDocument,
  type EventWithRegistrationsFragment,
  SetEventRegistrationDocument,
} from '@/graphql/Event';
import { DialogTitle } from '@/ui/dialog';
import { EventLoginPrompt } from '@/ui/EventLoginPrompt';
import { ComboboxSearchArea } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { formatParticipant } from '@/ui/format';
import { EventRegistrationForm } from '@/ui/forms/EventRegistrationForm';
import { Spinner } from '@/ui/Spinner';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
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

const registrant = (x: EventRegistrationFragment): Registrant => ({
  id: x.id,
  label: formatParticipant(x),
  personId: x.personId,
  coupleId: x.coupleId,
});

function useRegistrationCandidates(
  event: EventWithRegistrationsFragment | null | undefined,
  enabled: boolean,
) {
  const auth = useAuth();
  const isManager = !!event && auth.canManageEvent(event);

  const [query] = useQuery({
    query: EventFormOptionsDocument,
    variables: { forRegistration: true },
    pause: !isManager || !enabled,
  });
  const registeredPeople = new Set(
    (event?.registrationsList ?? []).flatMap((r) =>
      r.personId ? [r.personId] : r.couple ? [r.couple.manId, r.couple.womanId] : [],
    ),
  );

  const people = isManager ? (query.data?.people?.nodes ?? []) : auth.persons;
  const internalPersonIds = new Set(people.filter((x) => !x.isExternal).map((x) => x.id));
  const couples = isManager
    ? (query.data?.couplesList ?? [])
    : [...new Map(auth.couples.map((c) => [c.id, c])).values()].filter(
        (c) => c.status === 'ACTIVE',
      );

  const capacityLeft =
    event?.registrationInfo?.remainingCapacity ?? Number.POSITIVE_INFINITY;
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
          !registeredPeople.has(c.manId) &&
          !registeredPeople.has(c.womanId) &&
          (isManager || capacityLeft >= (event?.capacityUnit === 'PEOPLE' ? 2 : 1)),
      )
      .map((couple) => ({
        id: `couple:${couple.id}`,
        label: couple.name,
        isExternal: ![couple.manId, couple.womanId].some(
          (id) => !!id && internalPersonIds.has(id),
        ),
        personId: null,
        coupleId: couple.id,
      })),
  ]
    .filter(() => isManager || event?.registrationInfo?.canRegister)
    .toSorted((a, b) => {
      const aMine = auth.isMyPerson(a.personId) || auth.isMyCouple(a.coupleId);
      const bMine = auth.isMyPerson(b.personId) || auth.isMyCouple(b.coupleId);
      return (
        Number(a.isExternal) - Number(b.isExternal) ||
        Number(bMine) - Number(aMine) ||
        compareText(a.label, b.label)
      );
    });

  return {
    candidates,
    error: query.error,
    loading: isManager && query.fetching && !query.data,
  };
}

export function MyRegistrationsDialog({
  eventId,
  initialRegistrationId,
  initialPage,
}: {
  eventId: string;
  initialRegistrationId?: string;
  initialPage?: 'overview' | 'candidates' | 'new-person';
}) {
  const [query] = useQuery({
    query: EventRegistrationsDocument,
    variables: { id: eventId },
  });

  const event = query.data?.event;
  const auth = useAuth();
  const isManager = !!event && auth.canManageEvent(event);
  const { onSuccess: onClose } = useFormResult();
  const registrations = isManager
    ? (event?.registrationsList ?? [])
    : (event?.registrationsList ?? []).filter(
        (r) => auth.isMyPerson(r.personId) || auth.isMyCouple(r.coupleId),
      );
  const [selection, setSelection] = React.useState<Selection | null>(null);
  const registration =
    selection?.kind === 'registration'
      ? registrations.find((r) => r.id === selection.id)
      : undefined;
  const selected = registration
    ? registrant(registration)
    : selection?.kind === 'candidate'
      ? selection.registrant
      : undefined;
  const {
    candidates,
    error: candidateError,
    loading: candidatesLoading,
  } = useRegistrationCandidates(event, selection?.kind === 'candidates');
  const [setRegistrationResult, setRegistration] = useMutation(
    SetEventRegistrationDocument,
  );

  if (!query.data) return query.error ? <FormError error={query.error} /> : <Spinner />;
  if (!event) return <FormError error="Termín není dostupný." />;
  if (!selection) {
    const initial =
      registrations.find((r) => r.id === initialRegistrationId) ??
      (registrations.length === 1 ? registrations[0] : undefined);
    setSelection(
      initialPage
        ? { kind: initialPage }
        : initial
          ? { kind: 'registration', id: initial.id }
          : { kind: registrations.length > 0 ? 'overview' : 'candidates' },
    );
    return null;
  }

  const page = selected
    ? 'editor'
    : selection?.kind === 'candidates'
      ? !candidatesLoading && !candidateError && candidates.length === 0
        ? 'new-person'
        : 'candidates'
      : selection?.kind === 'new-person'
        ? 'new-person'
        : 'overview';
  const i = registration ? registrations.indexOf(registration) : -1;
  const canCycle = !!registration && registrations.length > 1;
  const simpleRegistration = event.type === 'LESSON' || event.type === 'GROUP';

  const selectCandidate = async (candidate?: Registrant) => {
    if (!candidate) return;
    if (!simpleRegistration) {
      setSelection({ kind: 'candidate', registrant: candidate });
      return;
    }
    const result = await setRegistration({
      input: {
        eventId: event.id,
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
          <button
            type="button"
            className={buttonCls({ size: 'icon', variant: 'outline' })}
            onClick={() => setSelection({ kind: 'candidates' })}
          >
            <ChevronLeft />
          </button>
        )}
        {page !== 'overview' && registrations.length > 0 && (
          <button
            type="button"
            className={buttonCls({ size: 'icon', variant: 'outline' })}
            onClick={() => setSelection({ kind: 'overview' })}
          >
            <List />
          </button>
        )}

        {canCycle && (
          <button
            type="button"
            className={buttonCls({ size: 'icon', variant: 'outline' })}
            onClick={() => changeRegistration(-1)}
          >
            <ChevronLeft />
          </button>
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
          <button
            type="button"
            className={buttonCls({ size: 'icon', variant: 'outline' })}
            onClick={() => changeRegistration(1)}
          >
            <ChevronRight />
          </button>
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
          <button
            type="button"
            className={buttonCls({ size: 'icon', variant: 'outline' })}
            onClick={() => setSelection({ kind: 'candidates' })}
          >
            <Plus />
          </button>
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
        <ComboboxSearchArea
          inline
          options={registrations.map(registrant)}
          onChange={(id) => {
            if (id) setSelection({ kind: 'registration', id });
          }}
        />
      ) : page === 'candidates' ? (
        <>
          <FormError error={candidateError || setRegistrationResult.error} />
          {candidatesLoading ? (
            <Spinner />
          ) : candidates.length > 0 ? (
            <ComboboxSearchArea
              inline
              options={candidates}
              searchable={isManager}
              disabled={setRegistrationResult.fetching}
              onChange={(id) => selectCandidate(candidates.find((x) => x.id === id))}
            />
          ) : (
            <div className="text-neutral-11">Není koho dalšího přihlásit.</div>
          )}
        </>
      ) : page === 'new-person' ? (
        <>
          <EventLoginPrompt eventId={event.id} />
          <EventSignupForm event={event} />
        </>
      ) : (
        selected && (
          <EventRegistrationForm
            key={`${selected.id}:${registration?.id ?? 'new'}`}
            event={event}
            participant={registration ?? selected}
            onSaved={() => {
              if (!registration) onClose();
            }}
            onCancelled={() => {
              const next = registrations.find((r) => r.id !== registration?.id);
              setSelection(
                next ? { kind: 'registration', id: next.id } : { kind: 'candidates' },
              );
            }}
          />
        )
      )}
    </>
  );
}
