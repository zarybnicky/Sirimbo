import {
  type EventInstancePageFragment,
  EventInstanceRegistrationsDocument,
} from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import { eventExternalRegistrationActions } from '@/lib/actions/eventInstance';
import { ActionRow } from '@/ui/ActionRow';
import { Spinner } from '@/ui/Spinner';
import { FormError } from '@/ui/form';
import { formatLongCoupleName } from '@/ui/format';
import { useQuery } from 'urql';

export function EventInstanceRegistrations({
  instance,
}: {
  instance: Pick<
    EventInstancePageFragment,
    'id' | 'eventExternalRegistrationsByInstanceIdList'
  >;
}) {
  const [registrationsQuery] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id: instance.id },
  });
  const registrations = registrationsQuery.data?.eventInstance?.registrations.nodes ?? [];
  const externalRegistrations = instance.eventExternalRegistrationsByInstanceIdList;
  const externalRegistrationActionMap = useActionMap(
    eventExternalRegistrationActions,
    externalRegistrations,
  );

  return (
    <div>
      <FormError error={registrationsQuery.error} />
      {registrationsQuery.fetching && !registrationsQuery.data && <Spinner />}
      {registrations.map((registration) => (
        <div key={registration.id} className="p-1">
          <div>
            {registration.person
              ? registration.person.name || ''
              : formatLongCoupleName(registration.couple)}
          </div>
          {(registration.note ||
            registration.eventLessonDemandsByRegistrationIdList.length > 0) && (
            <div className="ml-3">
              {registration.eventLessonDemandsByRegistrationIdList.map((demand) => (
                <div key={demand.id}>
                  {demand.lessonCount}x {demand.trainer?.person?.name}
                </div>
              ))}
              {registration.note}
            </div>
          )}
        </div>
      ))}
      {externalRegistrations.map((registration) => (
        <ActionRow
          key={registration.id}
          actions={externalRegistrationActionMap.get(registration.id)!}
        >
          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <div>
              {registration.prefixTitle} {registration.firstName} {registration.lastName}{' '}
              {registration.suffixTitle}
            </div>
            {registration.note && <div className="ml-3">{registration.note}</div>}
          </div>
        </ActionRow>
      ))}
    </div>
  );
}
