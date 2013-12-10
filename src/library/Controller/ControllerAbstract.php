<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Response;
use TKOlomouc\Utility\Request;

abstract class ControllerAbstract implements ControllerInterface
{
    protected $request;

    public function __construct(Request $request)
    {
        $this->request = $request;
    }

    protected function resolveLayout()
    {
        if ($this->request->getView() == 'plainhtml') {
            return 'Layout/Html';
        }
        return 'Layout/PrettyHtml';
    }

    abstract protected function checkPermissions();

    abstract public function view();
}
