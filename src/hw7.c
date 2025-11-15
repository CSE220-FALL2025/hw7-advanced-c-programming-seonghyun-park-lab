#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        bst_sf *node = malloc(sizeof(bst_sf));

        node->mat = mat;
        node->left_child = NULL;
        node->right_child = NULL;
        return node;
    }

    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    } else if (mat->name > root->mat->name) {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    return root;

}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) {
        return NULL;   // not found
    }

    if (name == root->mat->name) {
        return root->mat;   // found it
    }
    else if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    }
    else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }

    // 1. Free left subtree
    free_bst_sf(root->left_child);

    // 2. Free right subtree
    free_bst_sf(root->right_child);

    // 3. Free the matrix stored at this node
    free(root->mat);

    // 4. Free this BST node
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    // Allocate result matrix
    unsigned int r = mat1->num_rows;
    unsigned int c = mat1->num_cols;

    matrix_sf *res = malloc(sizeof(matrix_sf) + r * c * sizeof(int));

    // Temporary matrix name (non-alphabetical)
    res->name = '!';    

    res->num_rows = r;
    res->num_cols = c;

    // Perform element-wise addition
    unsigned int total = r * c;
    for (unsigned int i = 0; i < total; i++) {
        res->values[i] = mat1->values[i] + mat2->values[i];
    }
    return res;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
       // Dimensions:
    // mat1: r1 x c1
    // mat2: r2 x c2, and we know c1 == r2
    unsigned int r1 = mat1->num_rows;
    unsigned int c1 = mat1->num_cols;
    unsigned int c2 = mat2->num_cols;

    // Allocate result matrix: r1 x c2
    matrix_sf *res = malloc(sizeof(matrix_sf) + r1 * c2 * sizeof(int));
    
    // Temporary name (non-alphabetic so evaluate_expr_sf can free it later)
    res->name = '!';
    res->num_rows = r1;
    res->num_cols = c2;

    // Standard triple-loop matrix multiplication
    for (unsigned int i = 0; i < r1; i++) {
        for (unsigned int j = 0; j < c2; j++) {
            int sum = 0;
            for (unsigned int k = 0; k < c1; k++) {
                int a = mat1->values[i * c1 + k];
                int b = mat2->values[k * c2 + j];
                sum += a * b;
            }
            res->values[i * c2 + j] = sum;
        }
    }
    return res;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
        unsigned int r = mat->num_rows;
    unsigned int c = mat->num_cols;

    // Transpose dimensions: c x r
    matrix_sf *res = malloc(sizeof(matrix_sf) + r * c * sizeof(int));

    res->name = '!';  // temporary name (non-alphabetic)
    res->num_rows = c;
    res->num_cols = r;

    // For each (i, j) in original → goes to (j, i) in transpose
    for (unsigned int i = 0; i < r; i++) {
        for (unsigned int j = 0; j < c; j++) {
            // original index:  i * c + j
            // transposed index: j * r + i
            res->values[j * r + i] = mat->values[i * c + j];
        }
    }
    return res;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
        unsigned int num_rows, num_cols;
    int offset = 0;

    // 1. Read rows and cols, record how many characters were consumed in offset
    sscanf(expr, " %u %u %n", &num_rows, &num_cols, &offset);

    // 2. Move pointer to after the dimensions
    const char *p = expr + offset;

    // 3. Advance to '[' (we can assume it exists)
    while (*p != '[') {
        p++;
    }
    p++; // skip '['

    // 4. Allocate matrix with flexible array
    unsigned int total = num_rows * num_cols;
    matrix_sf *m = malloc(sizeof(matrix_sf) + total * sizeof(int));

    m->name = name;
    m->num_rows = num_rows;
    m->num_cols = num_cols;

    // 5. Read exactly total integers in row-major order
    for (unsigned int i = 0; i < total; i++) {
        // Skip spaces and semicolons between values
        while (isspace((unsigned char)*p) || *p == ';') {
            p++;
        }

        int val;
        int consumed = 0;
        sscanf(p, " %d %n", &val, &consumed);
        m->values[i] = val;
        p += consumed;
    }

    return m;
}
static int precedence(char op) {
    switch (op) {
        case '+': return 1;
        case '*': return 2;
        default:  return 0;
    }
}

char* infix2postfix_sf(char *infix) {
        if (infix == NULL) {
        return NULL;
    }

    size_t len = strlen(infix);

    /* postfix can never be longer than infix (minus parentheses), so len+1 is safe */
    char *postfix = malloc(len + 1);
    if (postfix == NULL) {
        return NULL;
    }

    /* operator stack (for +, *, and '(' ) */
    char *stack = malloc(len);
    if (stack == NULL) {
        free(postfix);
        return NULL;
    }

    int   top = -1;      /* stack top index */
    size_t k = 0;        /* postfix write index */

    for (size_t i = 0; i < len; i++) {
        char c = infix[i];

        /* skip whitespace just in case */
        if (c == ' ' || c == '\t' || c == '\n') {
            continue;
        }

        if ((c >= 'A' && c <= 'Z')) {
            /* matrix operand (single char name) */
            postfix[k++] = c;
        }
        else if (c == '(') {
            stack[++top] = c;   /* push '(' */
        }
        else if (c == ')') {
            /* pop until matching '(' */
            while (top >= 0 && stack[top] != '(') {
                postfix[k++] = stack[top--];
            }
            if (top >= 0 && stack[top] == '(') {
                top--;          /* pop '(' and discard */
            }
        }
        else if (c == '\'') {
            /* postfix unary operator with highest precedence:
               just append immediately */
            postfix[k++] = c;
        }
        else if (c == '+' || c == '*') {
            /* binary operator: pop operators of >= precedence */
            while (top >= 0 && stack[top] != '(' &&
                   precedence(stack[top]) >= precedence(c)) {
                postfix[k++] = stack[top--];
            }
            stack[++top] = c;   /* push this operator */
        }
        else {
            /* unexpected character – assignment probably guarantees this doesn't happen */
        }
    }

    /* pop any remaining operators */
    while (top >= 0) {
        if (stack[top] != '(') {
            postfix[k++] = stack[top];
        }
        top--;
    }

    postfix[k] = '\0';

    free(stack);   /* free all temp memory before returning */

    return postfix;  /* caller must free(this) later */
}

static int is_original_matrix(const matrix_sf *m) {
    return isalpha((unsigned char)m->name);
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {


    // 1. Convert infix expression to postfix.
    char *postfix = infix2postfix_sf(expr);

    size_t len = strlen(postfix);

    // 2. Allocate a stack of matrix_sf* pointers.
    matrix_sf **stack = malloc(len * sizeof(matrix_sf *));
    
    int top = -1;   // empty stack

    // 3. Scan the postfix expression left-to-right.
    for (size_t i = 0; postfix[i] != '\0'; i++) {
        char c = postfix[i];

        // skip any whitespace just in case
        if (c == ' ' || c == '\t' || c == '\n') {
            continue;
        }

        if (isalpha((unsigned char)c)) {
            // Operand: find matrix in BST and push pointer on stack.
            matrix_sf *m = find_bst_sf(c, root);
            // spec basically guarantees m != NULL
            stack[++top] = m;
        }
        else if (c == '\'') {
            // Unary transpose: pop one, transpose, free temp if needed, push result.
            matrix_sf *A = stack[top--];

            matrix_sf *R = transpose_mat_sf(A);   // returns a NEW matrix

            // If A was created during evaluation (non-alphabet name), free it.
            if (!is_original_matrix(A)) {
                free(A);
            }

            stack[++top] = R;
        }
        else if (c == '+' || c == '*') {
            // Binary operator: pop right then left.
            matrix_sf *B = stack[top--];   // right operand
            matrix_sf *A = stack[top--];   // left operand

            matrix_sf *R = NULL;
            if (c == '+') {
                R = add_mats_sf(A, B);     // NEW matrix
            } else { // c == '*'
                R = mult_mats_sf(A, B);    // NEW matrix
            }

            // Free temporaries that were produced earlier during evaluation.
            if (!is_original_matrix(A)) {
                free(A);
            }
            if (!is_original_matrix(B)) {
                free(B);
            }

            stack[++top] = R;
        }
        else {

        }
    }

    // 4. Stack should now have exactly one matrix: the result.
    matrix_sf *result = NULL;
    if (top >= 0) {
        result = stack[top];
        // Give it the final user-specified name.
        result->name = name;
    }

    // 5. Clean up everything except the result matrix.
    free(stack);
    free(postfix);

    return result;   // caller will eventually insert & later free via free_bst_sf
}

matrix_sf *execute_script_sf(char *filename) {
    if (filename == NULL) {
        return NULL;
    }

    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        // Could not open script file
        return NULL;
    }

    char   *line = NULL;   // getline will malloc/realloc this for us
    size_t  buf_size = 0;  // canonical usage: start at 0, let getline choose
    ssize_t len;

    bst_sf    *root = NULL;        // BST of all matrices created so far
    matrix_sf *last_created = NULL; // matrix from the last non-empty line

    while ((len = getline(&line, &buf_size, file)) != -1) {
        // Strip trailing newline, if present
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }

        // Skip leading whitespace
        char *p = line;
        while (isspace((unsigned char)*p)) {
            p++;
        }
        if (*p == '\0') {
            // Empty or whitespace-only line
            continue;
        }

        // Left-hand side matrix name (single char)
        char lhs_name = *p++;
        // Skip whitespace up to '='
        while (isspace((unsigned char)*p)) {
            p++;
        }
        p++;  // skip '=' (inputs are guaranteed well-formed)

        // Skip whitespace after '=' to get start of RHS
        while (isspace((unsigned char)*p)) {
            p++;
        }

        // p now points at the RHS of "X = RHS..."
        matrix_sf *m;

        if (isdigit((unsigned char)*p)) {
            m = create_matrix_sf(lhs_name, p);
        } else {
            m = evaluate_expr_sf(lhs_name, p, root);
        }

        // Insert new matrix into BST so later lines can reference it
        root = insert_bst_sf(m, root);

        // Track the last matrix created (the one we’ll return)
        last_created = m;
    }

    free(line);
    fclose(file);

    return last_created;   // matrix named on the last line of the script
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}

