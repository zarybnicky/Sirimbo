import React from 'react';
import * as Dialog from '@radix-ui/react-dialog';
import cx from 'classnames';
import { Edit3, X as CloseIcon } from 'lucide-react';
import { TextFieldElement } from '@app/ui/fields/text';
import { SubmitButton } from '@app/ui/submit';
import { useForm } from 'react-hook-form';
import { SubmitFormDocument } from '@app/graphql/Crm';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useCookie } from 'lib/use-cookie';
import { useMutation } from 'urql';
import { RichTextEditor } from './RichTextEditor';
import { DialogContent, DialogTitle } from '@app/ui/dialog';

function FeedbackForm() {
  const [isOpen, setIsOpen] = React.useState(false);
  const { control, handleSubmit } = useForm();
  const submit = useMutation(SubmitFormDocument)[1];
  const [isSubmitted, setSubmitted] = useCookie('feedback-submitted', 'true');

  const onSubmit = useAsyncCallback(async (data: any) => {
    const url = window.location.toString();
    await submit({ type: 'Zpětná vazba, web 05/2023', data, url });
    setIsOpen(false);
    setSubmitted('true', 30);
    toast.success('Děkujeme za odezvu!');
  });

  if (!!isSubmitted) {
    return <div key="feedback" />;
  }

  return (
    <div key="feedback" className="absolute bottom-4 right-4">
      <Dialog.Root open={isOpen} onOpenChange={setIsOpen}>
        <Dialog.Trigger asChild>
          <button
            className={cx(
              'text-white shadow-stone-700 bg-red-500 hover:bg-red-600 inline-flex h-[35px] items-center justify-center rounded-lg px-3 leading-none shadow-[0_2px_10px] focus:shadow-[0_0_0_2px] focus:shadow-black focus:outline-none',
            )}
          >
            <Edit3 className="h-4 w-4 mr-2" /> Ohodnoť nový web!
          </button>
        </Dialog.Trigger>

        <DialogContent>
            <DialogTitle className="text-neutral-12 mb-2 text-lg">
              <Edit3 className="inline h-4 w-4 mr-1" /> Ohodnoť nový web!
            </DialogTitle>

            <form className="grid gap-4" onSubmit={handleSubmit(onSubmit.execute)}>
              <TextFieldElement control={control} name="name" label="Jméno" />
              <TextFieldElement control={control} name="email" type="email" placeholder="@" label="E-mail" />
              <RichTextEditor
                control={control}
                name="feedback"
                label="Nedostatky, připomínky, chvála, ..."
              />
              <div className="flex justify-end">
                  <SubmitButton loading={onSubmit.loading} />
              </div>
            </form>

            <Dialog.Close asChild>
              <button
                className="text-red-500 hover:bg-stone-200 focus:shadow-red-700 absolute top-[10px] right-[10px] inline-flex h-[25px] w-[25px] appearance-none items-center justify-center rounded-full focus:shadow-[0_0_0_2px] focus:outline-none"
                aria-label="Close"
              >
                <CloseIcon className="h-4 w-4" />
              </button>
            </Dialog.Close>
        </DialogContent>
      </Dialog.Root>
    </div>
  );
}

export default FeedbackForm;