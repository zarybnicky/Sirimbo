import React from 'react';
import * as Dialog from '@radix-ui/react-dialog';
import cx from 'classnames';
import { Edit3, X as CloseIcon } from 'lucide-react';
import { TextFieldElement } from './TextField';
import { SubmitButton } from './SubmitButton';
import { useForm } from 'react-hook-form';
import { SubmitFormDocument } from '@app/graphql/Crm';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useCookie } from 'lib/use-cookie';
import { useMutation } from 'urql';
import { RichTextEditor } from './RichTextEditor';

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

        <Dialog.Portal>
          <Dialog.Overlay
            className={cx(
              'bg-black/50 fixed inset-0 z-20',
              'data-[state=open]:animate-overlayShow data-[state=closed]:animate-overlayHide',
            )}
          />
          <Dialog.Content
            className={cx(
              'fixed z-50 overflow-y-auto',
              'w-[95vw] max-h-[95vh] max-w-2xl rounded-lg p-4 md:w-full',
              'top-[50%] left-[50%] -translate-x-[50%] -translate-y-[50%]',
              'bg-white dark:bg-gray-800',
              'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
              'data-[state=open]:animate-contentShow data-[state=closed]:animate-contentHide',
            )}
          >
            <Dialog.Title className="text-stone-900 mb-2 text-lg">
              <Edit3 className="inline h-4 w-4 mr-1" /> Ohodnoť nový web!
            </Dialog.Title>

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
          </Dialog.Content>
        </Dialog.Portal>
      </Dialog.Root>
    </div>
  );
}

export default FeedbackForm;
