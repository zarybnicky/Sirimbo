import React from 'react';

type ErrorPageProps = {
  error: string;
  details?: React.ReactNode;
};

export function ErrorPage({ error, details }: ErrorPageProps) {
  return (
    <div className="flex h-[calc(100vh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
      <div className="text-center">
        <div className="inline-flex rounded-full bg-accent-4 p-4">
          <div className="rounded-full stroke-accent-10 bg-accent-6 p-4">
            <svg
              role="presentation"
              className="size-16"
              viewBox="0 0 28 28"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M14.0002 9.33337V14M14.0002 18.6667H14.0118M25.6668 14C25.6668 20.4434 20.4435 25.6667 14.0002 25.6667C7.55684 25.6667 2.3335 20.4434 2.3335 14C2.3335 7.55672 7.55684 2.33337 14.0002 2.33337C20.4435 2.33337 25.6668 7.55672 25.6668 14Z"
                strokeWidth="2"
                strokeLinecap="round"
                strokeLinejoin="round"
              />
            </svg>
          </div>
        </div>
        <h1 className="mt-5 text-[36px] font-bold text-neutral-12 lg:text-[50px]">
          {error}
        </h1>
        <p className="text-neutral-11 mt-5 lg:text-lg">{details}</p>
      </div>
    </div>
  );
}
