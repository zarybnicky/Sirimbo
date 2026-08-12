import { type EventPageFragment, EventRegistrationsDocument } from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import { canManageInstance, eventExternalRegistrationActions } from '@/lib/actions/eventInstance';
import { ActionRow } from '@/ui/ActionRow';
import { Dialog, DialogContent } from '@/ui/dialog';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { Spinner } from '@/ui/Spinner';
import { FormError } from '@/ui/form';
import { formatCoupleName } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { ChevronRight } from 'lucide-react';
import React from 'react';
import { useQuery } from 'urql';
import { useAuth } from './use-auth';

export function EventRegistrations({
  instance,
}: {
  instance: EventPageFragment;
}) {
  const auth = useAuth();
  const [selectedRegistrationId, setSelectedRegistrationId] = React.useState<string>();
  const [registrationsQuery] = useQuery({
    query: EventRegistrationsDocument,
    variables: { id: instance.id },
  });
  const registrations = registrationsQuery.data?.eventInstance?.registrationsList ?? [];
  const externalRegistrations = instance.eventExternalRegistrationsByInstanceIdList;
  const externalRegistrationActionMap = useActionMap(
    eventExternalRegistrationActions,
    externalRegistrations,
  );
  const isManager = canManageInstance({ auth, item: instance });

  return (
    <div>
      <FormError error={registrationsQuery.error} />
      {registrationsQuery.fetching && !registrationsQuery.data && <Spinner />}
      {externalRegistrations.length > 0 && registrations.length > 0 && (
        <h3 className="mb-1 font-medium text-neutral-11">Přihlášky členů</h3>
      )}
      {registrations.map((r) => {
        const canEdit = isManager || auth.isMyPerson(r.personId) || auth.isMyCouple(r.coupleId);
        return (
          <button
            key={r.id}
            type="button"
            disabled={!canEdit}
            className={buttonCls({
              display: 'listItem',
              variant: 'none',
              size: 'none',
              className:
                'items-start border-b border-neutral-5 last:border-0 disabled:text-inherit disabled:hover:bg-transparent',
            })}
            onClick={() => setSelectedRegistrationId(r.id)}
          >
            <div className="min-w-0 grow">
              <div className="font-medium text-neutral-12">
                {r.person
                  ? r.person.name || ''
                  : formatCoupleName(r.couple)}
              </div>
              {auth.isTrainerOrAdmin && (
                <div className="mt-1 text-sm text-neutral-11">
                  {r.eventLessonDemandsByRegistrationIdList.map((demand) => (
                    <div key={demand.id}>
                      {demand.lessonCount}× {demand.trainer?.person?.name}
                    </div>
                  ))}
                  {r.note && (
                    <div className="whitespace-pre-wrap">{r.note}</div>
                  )}
                </div>
              )}
            </div>
            {canEdit && <ChevronRight className="size-4 shrink-0 text-neutral-9" />}
          </button>
        );
      })}
      {externalRegistrations.length > 0 && (
        <h3 className="mb-1 mt-4 font-medium text-neutral-11">Externí přihlášky</h3>
      )}
      {externalRegistrations.map((r) => (
        <ActionRow
          key={r.id}
          actions={externalRegistrationActionMap.get(r.id)!}
          className="mb-0 items-start border-b border-neutral-5 py-2 last:border-0"
        >
          <div className="min-w-0 grow">
            <div className="font-medium text-neutral-12">
              {r.prefixTitle} {r.firstName} {r.lastName}{' '}
              {r.suffixTitle}
            </div>
            {auth.isTrainerOrAdmin && r.note && (
              <div className="mt-1 whitespace-pre-wrap text-sm text-neutral-11">
                {r.note}
              </div>
            )}
          </div>
        </ActionRow>
      ))}
      {selectedRegistrationId && (
        <Dialog open onOpenChange={() => setSelectedRegistrationId(undefined)}>
          <DialogContent>
            <MyRegistrationsDialog
              instance={instance}
              isManager={isManager}
              initialRegistrationId={selectedRegistrationId}
            />
          </DialogContent>
        </Dialog>
      )}
    </div>
  );
}
