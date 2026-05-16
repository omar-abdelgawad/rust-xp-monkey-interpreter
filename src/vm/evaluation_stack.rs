use std::collections::HashMap;

use crate::object::{garbage_obj, Hashable, IsHashable, ObjRef, Object};

#[derive(Debug)]
pub struct EvaluationStack {
    stack: Vec<ObjRef>,
    sp: usize, // always points to next value. top of stack is stack[sp -1]
}

const STACKSIZE: usize = 1 << 11;

impl EvaluationStack {
    pub fn new() -> Self {
        Self {
            stack: vec![garbage_obj(); STACKSIZE],
            sp: 0,
        }
    }

    pub fn last_popped(&self) -> ObjRef {
        self.stack[self.sp].clone()
    }

    // TODO: is this function really required? shouldn't it just be like extract_arr?
    pub fn extract_vec(&mut self, length: usize) -> Vec<ObjRef> {
        let mut free = Vec::with_capacity(length);
        // TODO: a lot of unnecessary cloning here
        for i in 0..length {
            free.push(self.stack[self.sp - length + i].clone());
        }
        self.sp -= length;
        free
    }

    pub fn extract_arr(&mut self, num_elements: usize) -> ObjRef {
        let start_ind = self.sp - num_elements;
        let end_ind = self.sp;
        let elements = self.stack[start_ind..end_ind].to_vec();
        self.sp -= num_elements;
        Object::new_array_var(elements)
    }
    //TODO: How could this extraction fail? Is it possible to return only ObjRef?
    pub fn extract_hash(&mut self, num_elements: usize) -> Result<ObjRef, String> {
        let start_ind = self.sp - num_elements;
        let end_ind = self.sp;
        let mut hashed_pairs = HashMap::new();
        let mut i = start_ind;
        while i < end_ind {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            // TODO: this hash_key should return result instead
            key.is_hashable()?;
            let hash_key = key.hash_key();

            let pair = crate::object::HashPair::new(key, value);
            hashed_pairs.insert(hash_key, pair);

            i += 2;
        }
        self.sp -= num_elements;
        Ok(Object::new_hash_var(hashed_pairs))
    }

    pub fn sp_mut(&mut self) -> &mut usize {
        &mut self.sp
    }

    pub fn sp(&self) -> &usize {
        &self.sp
    }

    pub fn push(&mut self, o: ObjRef) -> Result<(), String> {
        if self.sp >= STACKSIZE {
            Err("woops! stack overflow".to_string())
        } else {
            self.stack[self.sp] = o;
            self.sp += 1;
            Ok(())
        }
    }

    pub fn pop(&mut self) -> ObjRef {
        let o = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        o
    }

    pub fn set_variable(&mut self, ind: usize, obj: ObjRef) {
        self.stack[ind] = obj;
    }

    pub fn get_variable(&self, ind: usize) -> ObjRef {
        self.stack[ind].clone()
    }

    pub fn get_callee(&self, num_args: usize) -> ObjRef {
        self.stack[self.sp - 1 - num_args].clone()
    }

    pub fn extract_slice(&mut self, num_args: usize) -> &[ObjRef] {
        let args = &self.stack[self.sp - num_args..self.sp];
        self.sp = self.sp - num_args - 1;
        args
    }
}
