import {
  type EventInstancePageFragment,
  EventInstanceRegistrationsDocument,
} from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import {
  canManageInstance,
  eventExternalRegistrationActions,
} from '@/lib/actions/eventInstance';
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
  instance: EventInstancePageFragment;
}) {
  const auth = useAuth();
  const [selectedRegistrationId, setSelectedRegistrationId] = React.useState<string>();
  const [registrationsQuery] = useQuery({
    query: EventInstanceRegistrationsDocument,
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
      {registrations.map((registration) => {
        const canEdit =
          isManager ||
          auth.isMyPerson(registration.personId) ||
          auth.isMyCouple(registration.coupleId);
        return (
          <button
            key={registration.id}
            type="button"
            disabled={!canEdit}
            className={buttonCls({
              display: 'listItem',
              variant: 'none',
              size: 'none',
              className:
                'items-start border-b border-neutral-5 last:border-0 disabled:text-inherit disabled:hover:bg-transparent',
            })}
            onClick={() => setSelectedRegistrationId(registration.id)}
          >
            <div className="min-w-0 grow">
              <div className="font-medium text-neutral-12">
                {registration.person
                  ? registration.person.name || ''
                  : formatCoupleName(registration.couple)}
              </div>
              {auth.isTrainerOrAdmin && (
                <div className="mt-1 text-sm text-neutral-11">
                  {registration.eventLessonDemandsByRegistrationIdList.map((demand) => (
                    <div key={demand.id}>
                      {demand.lessonCount}× {demand.trainer?.person?.name}
                    </div>
                  ))}
                  {registration.note && (
                    <div className="whitespace-pre-wrap">{registration.note}</div>
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
      {externalRegistrations.map((registration) => (
        <ActionRow
          key={registration.id}
          actions={externalRegistrationActionMap.get(registration.id)!}
          className="mb-0 items-start border-b border-neutral-5 py-2 last:border-0"
        >
          <div className="min-w-0 grow">
            <div className="font-medium text-neutral-12">
              {registration.prefixTitle} {registration.firstName} {registration.lastName}{' '}
              {registration.suffixTitle}
            </div>
            {auth.isTrainerOrAdmin && registration.note && (
              <div className="mt-1 whitespace-pre-wrap text-sm text-neutral-11">
                {registration.note}
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
