import {
  type EventInstanceRegistrationFragment,
  type EventWithTrainerFragment,
  type EventInstanceTrainerLessonOfferFragment,
  SetEventInstanceRegistrationDocument,
} from '@/graphql/Event';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { dateTimeFormatter, formatRegistrant } from '@/ui/format';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { MyRegistrationForm } from '@/ui/forms/MyRegistrationForm';
import { FormError } from '@/ui/form';
import { useConfirm } from './Confirm';
import { SubmitButton } from './submit';
import { cardCls } from './style';
import { useAtomValue } from 'jotai';
import { tenantIdAtom } from '@/ui/state/auth';

export function MyRegistrationCard({
  instance,
  registration,
  lessonTrainers,
}: {
  instance: EventWithTrainerFragment;
  registration: EventInstanceRegistrationFragment;
  lessonTrainers: EventInstanceTrainerLessonOfferFragment[];
}) {
  const tenantId = useAtomValue(tenantIdAtom);
  const confirm = useConfirm();
  const cancel = useMutation(SetEventInstanceRegistrationDocument)[1];
  const onCancel = useAsyncCallback(async () => {
    await confirm({ description: 'Opravdu chcete zrušit přihlášku?' });
    const result = await cancel({
      input: {
        pInstanceId: instance.id,
        pPersonId: registration.personId,
        pCoupleId: registration.coupleId,
        pIsRegistered: false,
      },
    });
    if (result.error) throw result.error;
    toast.success('Přihláška zrušena.');
  });

  return (
    <div className={cardCls({ className: 'prose prose-accent' })}>
      <h5>{formatRegistrant(registration)}</h5>
      <div>
        Přihlášeno
        {' v '}
        {dateTimeFormatter.format(new Date(registration.createdAt))}
      </div>
      {registration.eventLessonDemandsByRegistrationIdList.length > 0 && (
        <div>
          <h5>Požadavky na lekce</h5>
          <ul>
            {registration.eventLessonDemandsByRegistrationIdList.map((demand) => (
              <li key={demand.id}>
                {demand.trainer?.person?.name}: {demand.lessonCount}
              </li>
            ))}
          </ul>
        </div>
      )}
      {registration.note && <p>{registration.note}</p>}
      <FormError error={onCancel.error} />

      {(instance.enableNotes ||
        registration.note ||
        registration.eventLessonDemandsByRegistrationIdList.length > 0 ||
        lessonTrainers.length > 0) && (
        <Dialog>
          <DialogTrigger text="Upravit přihlášku" />
          <DialogContent>
            <MyRegistrationForm
              instanceId={instance.id}
              enableNotes={!!instance.enableNotes}
              registration={registration}
              lessonTrainers={lessonTrainers}
            />
          </DialogContent>
        </Dialog>
      )}

      {tenantId === '2' ? null : (
        <SubmitButton
          type="button"
          variant="outline"
          onClick={onCancel.execute}
          loading={onCancel.loading}
        >
          Zrušit přihlášku
        </SubmitButton>
      )}
    </div>
  );
}
