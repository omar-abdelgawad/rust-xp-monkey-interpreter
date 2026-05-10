use crate::code::Instructions;
use crate::object::ClosureObj;

#[derive(Debug, Clone)]
pub(super) struct Frame {
    pub cl: ClosureObj,
    pub ip: i64,
    pub bp: i64,
}
impl Frame {
    pub fn new(cl: ClosureObj, bp: i64) -> Self {
        Self { cl, ip: -1, bp }
    }

    //fn instructions_mut(&mut self) -> &mut Instructions {
    //    &mut self.cl.comp_fn.instructions
    //}

    pub fn instructions(&self) -> &Instructions {
        &self.cl.comp_fn.instructions
    }
}
