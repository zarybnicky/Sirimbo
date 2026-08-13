import * as React from 'react';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { toast } from 'react-toastify';
import { Dialog, DialogContent } from '@/ui/dialog';
import { EditPersonForm } from '@/ui/forms/EditPersonForm';
import { buttonCls } from './style';
import { Edit } from 'lucide-react';

export function FillYourProfileReminder() {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [personId, setPersonId] = React.useState<string | null>(null);

  React.useEffect(() => {
    if (authLoading) return;

    const now = Date.now() - 24 * 60 * 60 * 1000;
    const shouldCheck = now - 24 * 60 * 60 * 1000;
    for (const person of auth.persons) {
      if (person.externalIds) continue;

      const lastChecked = localStorage.getItem(`profile-checked-${person.id}`);
      if (lastChecked && lastChecked >= shouldCheck.toString()) continue;

      localStorage.setItem(`profile-checked-${person.id}`, now.toString());
      if (
        person.email &&
        person.phone &&
        person.nationality &&
        person.birthDate &&
        person.taxIdentificationNumber
        /* person.address?.street &&
         * person.address?.city &&
         * person.address?.postalCode */
      )
        continue;

      toast.warn(
        <>
          Vyplňte prosím chybějící údaje u osoby {person.name}:
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => setPersonId(person.id)}
          >
            <Edit />
            Upravit osobu
          </button>
        </>,
      );
    }
  }, [auth.persons, authLoading]);

  return (
    <Dialog open={!!personId} onOpenChange={() => setPersonId(null)}>
      {personId && (
        <DialogContent
          className="sm:max-w-2xl"
          onPointerDownOutside={(e) => e.preventDefault()}
        >
          <EditPersonForm id={personId} />
        </DialogContent>
      )}
    </Dialog>
  );
}
